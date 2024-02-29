module Window = Brr_canvas.Canvas
module C = Brr_canvas.C2d

let global_canvas : Window.t option ref = ref None
let global_ctx : C.t option ref = ref None
let render () = Option.get !global_ctx
