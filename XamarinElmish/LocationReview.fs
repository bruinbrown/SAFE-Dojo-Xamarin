module LocationReview

open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open Xamarin.Forms
open Newtonsoft.Json
open Xamarin.Forms.Maps

type Xaml =
    static member BarChart(elements:Microcharts.Entry seq) =
        let create () =
            let chart = Microcharts.BarChart(Entries = elements)
            chart.LabelTextSize <- 40.f
            let chartView = Microcharts.Forms.ChartView(Chart = chart)
            chartView.HeightRequest <- 500.
            box chartView

        let update prevAttribs source target =
            ()

        XamlElement(typeof<Microcharts.Chart>, create, update, Map.empty)

    static member Map(latitude, longitude) =
        let create () =
            let position = Position(latitude, longitude)
            let map = Map(MapSpan.FromCenterAndRadius(position, Distance(500.)))
            map.HeightRequest <- 200.
            //map.Pins.Add(Pin(Position = position))
            box map

        let update prevAttribs source target =
            ()

        XamlElement(typeof<Map>, create, update, Map.empty)

module Validation =
    open System.Text.RegularExpressions
    let validatePostcode postcode =
        Regex.IsMatch(postcode, @"([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\s?[0-9][A-Za-z]{2})")

type LatLong =
    { Latitude : float
      Longitude : float }
type Location =
    { Town : string
      Region : string
      LatLong : LatLong }
type GetLocationRequest = { Postcode : string }
type LocationResponse = { Postcode : string; Location : Location; DistanceToLondon : float }
type CrimeResponse = { Crime : string; Incidents : int }

type WeatherType =
    | Snow
    | Sleet
    | Hail
    | Thunder
    | HeavyRain
    | LightRain
    | Showers
    | HeavyCloud
    | LightCloud
    | Clear
    static member Parse =
        let weatherTypes = FSharp.Reflection.FSharpType.GetUnionCases typeof<WeatherType>
        fun (s:string) ->
            weatherTypes
            |> Array.find(fun w -> w.Name = s.Replace(" ", ""))
            |> fun u -> FSharp.Reflection.FSharpValue.MakeUnion(u, [||]) :?> WeatherType
    member this.Abbreviation =
        match this with
        | Snow -> "sn" | Sleet -> "s" | Hail -> "h" | Thunder -> "t" | HeavyRain -> "hr"
        | LightRain -> "lr" | Showers -> "s" | HeavyCloud -> "hc" | LightCloud -> "lc" | Clear -> "c"

type WeatherResponse = { WeatherType : WeatherType; AverageTemperature : float }

type Report =
    { Location : LocationResponse
      Crimes : CrimeResponse array
      Weather : WeatherResponse }

type ServerState = Idle | Loading | ServerError of string

/// The overall data model driving the view.
type Model =
    { Postcode : string
      ValidationError : string option
      ServerState : ServerState
      Report : Report option }

type Msg =
    | GetReport
    | PostcodeChanged of string
    | GotReport of Report
    | ErrorMsg of exn
    | ClearResult

/// The init function is called to start the message pump with an initial view.
let init () =
    { Postcode = null
      Report = None
      ValidationError = None
      ServerState = Idle }, Cmd.ofMsg (PostcodeChanged "")

let getResponse postcode = async {
    let jsonSettings = JsonSerializerSettings()
    jsonSettings.Converters.Add(Fable.JsonConverter())
    let wc = new System.Net.WebClient()
    let request = JsonConvert.SerializeObject({ Postcode = postcode }, jsonSettings)
    wc.Headers.Add("Content-Type", "application/json")
    let! location = wc.UploadStringTaskAsync("http://localhost:8080/api/distance", request) |> Async.AwaitTask
    let location = JsonConvert.DeserializeObject<LocationResponse>(location, jsonSettings)

    let! crimes = wc.DownloadStringTaskAsync(sprintf "http://localhost:8080/api/crime/%s" postcode) |> Async.AwaitTask
    let crimes = JsonConvert.DeserializeObject<CrimeResponse array>(crimes, jsonSettings)

    let! weather = wc.DownloadStringTaskAsync(sprintf "http://localhost:8080/api/weather/%s" postcode) |> Async.AwaitTask
    let weather = JsonConvert.DeserializeObject<WeatherResponse>(weather, jsonSettings)

    return GotReport({ Location = location; Crimes = crimes; Weather = weather }) }

let update msg model =
    match model, msg with
    | { ValidationError = None; Postcode = postcode }, GetReport ->
        { model with ServerState = Loading }, Cmd.ofAsyncMsg(getResponse postcode)
    | _, GetReport -> model, Cmd.none
    | _, GotReport response ->
        { model with
            ValidationError = None
            Report = Some response
            ServerState = Idle }, Cmd.none
    | _, PostcodeChanged p ->
        let p = p.ToUpper()
        { model with
            Postcode = p
            ValidationError =
              if Validation.validatePostcode p then None
              else Some "Invalid postcode." }, Cmd.none
    | _, ErrorMsg e -> { model with ServerState = ServerError e.Message }, Cmd.none
    | _, ClearResult -> { model with Report = None; Postcode = "" }, Cmd.none

let locationView (location : LocationResponse) =
    Xaml.StackLayout(
        children = [
            yield Xaml.Label(text = sprintf "%s is in %s, %s. It's %.1fKM from London" location.Postcode location.Location.Town location.Location.Region location.DistanceToLondon)
            yield Xaml.Map(location.Location.LatLong.Latitude, location.Location.LatLong.Longitude)
        ]
    )

let weatherView (weatherReport : WeatherResponse) =
    Xaml.StackLayout(
        children = [
            yield Xaml.Image(source = sprintf "https://www.metaweather.com/static/img/weather/png/%s.png" weatherReport.WeatherType.Abbreviation, horizontalOptions = LayoutOptions.CenterAndExpand)
            yield Xaml.Label(text = sprintf "%.1f°C" weatherReport.AverageTemperature, fontSize = 36., horizontalOptions = LayoutOptions.CenterAndExpand)
        ]
    )

let crimeView (crimeReport : CrimeResponse array) =
    let entries =
        crimeReport
        |> Array.map (fun cr -> Microcharts.Entry(cr.Incidents |> float32, Label = cr.Crime))
    Xaml.StackLayout(
        children = [
            yield Xaml.BarChart(entries)
        ]
    )

let view (model : Model) dispatch =
    Xaml.ContentPage(
        content=
            Xaml.ScrollView(
                content =
                    Xaml.StackLayout(
                    padding = 20.,
                    children = [
                        yield Xaml.Entry(text = model.Postcode, textChanged = fixf (fun args -> dispatch (PostcodeChanged args.NewTextValue)))
                        match model.ValidationError with
                        | Some validationError ->
                            yield Xaml.Label(text = validationError)
                        | None -> ()
                        yield Xaml.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            children = [
                                Xaml.Button("Get details", command = fixf (fun () -> dispatch GetReport), horizontalOptions = LayoutOptions.CenterAndExpand)
                                Xaml.Button("Clear report", command = fixf (fun () -> dispatch ClearResult), horizontalOptions = LayoutOptions.CenterAndExpand)
                            ])
                        match model with
                        | { Report = None; ServerState = Idle | Loading } -> ()
                        | { Report = None; ServerState = ServerError error } ->
                            yield Xaml.Label(text = "An error occured whilst retrieving data from the server")
                        | { Report = Some report; } ->
                            yield locationView report.Location
                            yield weatherView report.Weather
                            yield crimeView report.Crimes
                    ]
                )
        )
    )