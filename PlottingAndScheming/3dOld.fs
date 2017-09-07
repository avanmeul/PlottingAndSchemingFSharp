module PlottingAndScheming.ThreeD

open Microsoft.FSharp.Math
open System
open System.Collections.Generic
open System.ComponentModel 
open System.IO
open System.IO.IsolatedStorage
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Media.Media3D
open System.Windows.Shapes
open System.Xml.Linq

//; 3d functions follow.
//
//(define abs-x-*-abs-y
//  (lambda (x y)
//    (* (abs x) (abs y))))
//
//(define log-2-*-x^2-+-y^2
//  (lambda (x y)
//    (log (+ (* 2 x x) (* y y)))))
//
//(define sin-x-*-cos-y
//  (lambda (x y)
//    (* (sin x) (cos y))))
//
//;(plot3d abs-x-*-abs-y
//;        -3 3 -3 3 21 21 '((-.6 .8 0.0) (-.5 -.4 .8) (.6 .5 .6)) 'half)
//
//;(make-3d-plot-object abs-x-*-abs-y -3 3 -3 3 21 21) 
//
//;(show-plot-object foo 'half () #t #t () #t () '((-.6 .8 0.0) (-.5 -.4 .8) (.6 .5 .6)))

let absXabsY x y = (abs x) * (abs y)



let threeDTabItem () =
    let tabItem = new TabItem()
    tabItem.Name <- "tab3d"
    tabItem.Header <- "3d"
    let pnl = new StackPanel ()
    let btn = new Button ()
    btn.Height <- 25.0
    btn.Content <- "Plot"
    btn.Click.Add (
        fun _ ->
            let viewport = new Viewport3D ()
            let mesh = new MeshGeometry3D ()
            mesh.Positions.Add (new Point3D (0.0, 0.0, 0.0))
            mesh.Positions.Add (new Point3D (0.0, 1.0, -1.0))
            mesh.Positions.Add (new Point3D (0.0, 0.0, -2.0))
            let col = {0..2}
            mesh.TriangleIndices <- new Int32Collection (col)
            //Define the GeometryModel3D.
            let geomod = new GeometryModel3D ()
            geomod.Geometry <- mesh
            let mat = new DiffuseMaterial (Brushes.Cyan) :> Material            
            geomod.Material <- mat
            geomod.BackMaterial <- (new DiffuseMaterial (Brushes.Red) :> Material)
            // Create ModelVisual3D for GeometryModel3D.
            let modvis = new ModelVisual3D ()
            modvis.Content <- geomod
            viewport.Children.Add modvis
            // Create another ModelVisual3D for light.
            let modvis = new ModelVisual3D ()
            modvis.Content <- new AmbientLight (Colors.White)
            viewport.Children.Add modvis
            // Create the camera.
            let cam = new PerspectiveCamera(new Point3D(-2.0, 0.0, 5.0), new Vector3D(0.0, 0.0, -1.0), new Vector3D(0.0, 1.0, 0.0), 45.0)
            viewport.Camera <- cam
            let win = new Window ()
            win.Content <- viewport
            win.Show ()
            )
    pnl.Children.Add btn |> ignore
    tabItem.Content <- pnl
    tabItem