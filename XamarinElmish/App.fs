namespace XamarinElmish

open Xamarin.Forms
open Elmish.XamarinForms
open LocationReview

type App() as app =
    inherit Application()

    let runner =
        Program.mkProgram init update view
        |> Program.withConsoleTrace
        |> Program.withDynamicView app
        |> Program.run

    //let stack = StackLayout(VerticalOptions = LayoutOptions.Center)
    //let label = Label(XAlign = TextAlignment.Center, Text = "Welcome to F# Xamarin.Forms!")
    //do
        //stack.Children.Add(label)
        //base.MainPage <- ContentPage(Content = stack)
