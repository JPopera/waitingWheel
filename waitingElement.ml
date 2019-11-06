open Code
open Universe
open Image
open Color
open World
open List
open Float




type model = { time      : float
             ; objs      : Image.t list
             ; locations : (float * float) list
             ; counter   : float
             }



let displayHeight = 800.
let displayWidth = 800.
let fps = 32.
let timeEncrement = 1. /. fps
let circleOrbitTime = 20.
let circleRadius = 5.
let elementRadius = 100. /. 3.
let circle = Image.circle circleRadius Color.gray
let cycleTime = 10.0 (*circleOrbitTime *. 0.75 +. circleOrbitTime*)
let centerDisplacementX = displayWidth /. 2. -. circleRadius /. 2.
let centerDisplacementY = displayHeight /. 2. -. elementRadius -. circleRadius /. 2.
let angleAdjustmentY = elementRadius -. centerDisplacementY
let angleAdjustmentX = -.(displayWidth /. 2.) -. circleRadius /. 2.

let rec fillCircleList numberOfCircles =
  match numberOfCircles = 0 with
  | true -> []
  | false -> circle :: fillCircleList (numberOfCircles - 1)

(* no longer in use, but going to hold on to it
let rec updateLocation locationList =
  match locationList with
  | [] -> []
  | (x, y)::t ->
    let newAngle = atan ((y +. angleAdjustmentY) /. (x +. angleAdjustmentX)) +. timeEncrement *. Code.pi *. 2. in
    (elementRadius *. cos (newAngle) +. displayWidth /. 2. -. circleRadius /. 2.,
     elementRadius *. sin (newAngle) +. displayHeight /. 2. -. circleRadius /. 2.)
    :: updateLocation t
*)




let circleDeterminer time =
  if time < 5.497787143782138
  then Code.f2I(Float.floor(time *. 4. /. Code.pi)) + 1
  else 14 - Code.f2I(Float.floor(time *. 4. /. Code.pi))

let rec timeUpdate time locationList =
  let counter = if time >= 5.497787143782138 then Code.i2F (7 - (circleDeterminer time)) else 0. in
  let rec loop locationList counter =
    match locationList with
    | [] -> []
    | (x, y)::t ->
      (elementRadius *. cos (-.Code.pi /. 2. +. time -. Code.pi /. 4. *. counter) +. displayWidth /. 2. -. circleRadius /. 2.,
       elementRadius *. sin (-.Code.pi /. 2. +. time -. Code.pi /. 4. *. counter) +. displayHeight /. 2. -. circleRadius /. 2.)
      :: loop t (counter +. 1.)
  in
  loop locationList counter

let hidingCircles time =
  let counter = 7 - (circleDeterminer time) in
  let rec loop counter =
    match counter = 0 with
    | true -> []
    | false -> (centerDisplacementX, centerDisplacementY) :: loop (counter - 1)
  in
  loop counter


let length aList =
  let counter = 0 in
  let rec loop aList counter =
    match aList with
    | [] -> counter
    | h::t -> loop t (counter + 1)
  in
  loop aList counter

let rec fillLocationList x =
  match x = 0 with
  | true -> []
  | false -> (0., 0.) :: fillLocationList (x - 1)

let view model =
  let background = Image.place_image circle (centerDisplacementX, centerDisplacementY) (Image.rectangle displayHeight displayWidth Color.black)
  in
  Image.place_images model.objs model.locations background

let initialModel = {time = 0. ; objs = [circle] ; locations = [(centerDisplacementX +. 0.001, centerDisplacementY)] ; counter = 0.}

let update model =
  if model.time +. timeEncrement *. Code.pi *. 2. < 2. *. Code.pi *. 0.75 +. 2. *. Code.pi
  then
  let newTime = model.time +. timeEncrement *. Code.pi *. 2. in
  let circleList = fillCircleList (circleDeterminer newTime) in
  let location = timeUpdate newTime (fillLocationList (circleDeterminer newTime))
  in
  World.World { time = newTime
              ; objs = circleList
              ; locations = location
              ; counter = model.counter +. 1.
              }
  else World.World {time = 0. ; objs = [circle] ; locations = [(centerDisplacementX +. 0.001, centerDisplacementY)] ; counter = 0.}


let go model =
  World.big_bang model
    ~name: "Waiting Circles"
    ~width: 800
    ~height: 800
    ~to_draw: view
    ~on_tick: update
    ~rate: (1. /. 24.)
;;

let start = go initialModel
;;
start
