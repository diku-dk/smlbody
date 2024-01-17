fun reduce f n l =
  List.foldl f n l
fun map2 f nil _ = nil
  | map2 f _ nil = nil
  | map2 f (x :: xs) (y :: ys) =
      f x y :: map2 f xs ys

structure V =
struct
  type vec = real * real * real
  fun bin f ((x1, y1, z1): vec, (x2, y2, z2): vec) : vec =
    (f (x1, x2), f (y1, y2), f (z1, z2))
  fun dot x y : real =
    let val (a, b, c) = bin (op*) (x, y)
    in a + b + c
    end
  val op+ = bin (op+)
  val op- = bin (op-)
  fun const v : vec = (v, v, v)
  val zero = const 0.0
  fun scale s (x, y, z) : vec = (s * x, s * y, s * z)
end

type mass = real
type pos = V.vec
type acc = V.vec
type vel = V.vec
type body = {pos: pos, mass: mass, vel: vel}

structure Gui =
struct
  type rot = V.vec * V.vec * V.vec
  fun rotate_pos (rot: rot) (x, y, z) : pos =
    ( x * #1 (#1 rot) + y * #1 (#2 rot) + z * #1 (#3 rot)
    , x * #2 (#1 rot) + y * #2 (#2 rot) + z * #2 (#3 rot)
    , x * #3 (#1 rot) + y * #3 (#2 rot) + z * #3 (#3 rot)
    )

  fun rotate_poss (rot: rot) (poss: pos list) : pos list =
    map (rotate_pos rot) poss

  fun rotate_x_matrix (angle: real) : rot =
    ( (1.0, 0.0, 0.0)
    , (0.0, Math.cos angle, ~(Math.sin angle))
    , (0.0, Math.sin angle, Math.cos angle)
    )

  fun rotate_y_matrix (angle: real) : rot =
    ( (Math.cos angle, 0.0, Math.sin angle)
    , (0.0, 1.0, 0.0)
    , (~(Math.sin angle), 0.0, Math.cos angle)
    )

  fun rotmult (x: rot) (y: rot) : rot =
    ( ( V.dot (#1 (#1 x), #1 (#2 x), #1 (#3 x)) (#1 y)
      , V.dot (#2 (#1 x), #2 (#2 x), #2 (#3 x)) (#1 y)
      , V.dot (#3 (#1 x), #3 (#2 x), #3 (#3 x)) (#1 y)
      )
    , ( V.dot (#1 (#1 x), #1 (#2 x), #1 (#3 x)) (#2 y)
      , V.dot (#2 (#1 x), #2 (#2 x), #2 (#3 x)) (#2 y)
      , V.dot (#3 (#1 x), #3 (#2 x), #3 (#3 x)) (#2 y)
      )
    , ( V.dot (#1 (#1 x), #1 (#2 x), #1 (#3 x)) (#3 y)
      , V.dot (#2 (#1 x), #2 (#2 x), #2 (#3 x)) (#3 y)
      , V.dot (#3 (#1 x), #3 (#2 x), #3 (#3 x)) (#3 y)
      )
    )

  fun rotation (xrot: real) (yrot: real) : rot =
    rotmult (rotate_x_matrix xrot) (rotate_y_matrix yrot)

  fun inv_rotation (xrot: real) (yrot: real) : rot =
    rotmult (rotate_y_matrix yrot) (rotate_x_matrix xrot)

  fun render_body (h: int) (w: int) (x_ul: real, y_ul: real)
    (x_br: real, y_br: real) (max_mass: real)
    ({pos = (x, y, z), mass = m, ...}: body) : ((int * int) * Tigr.color) option =
    if x < x_ul orelse x > x_br orelse y < y_ul orelse y > y_br then
      NONE
    else
      let (* Normalise x,y to positions in interval (0,1) within the viewport. *)
        val x' = (x - x_ul) / (x_br - x_ul)
        val y' = (y - y_ul) / (y_br - y_ul)
        (* Convert x',y' to screen coordinate space. *)
        val x'' = floor (x' * real w)
        val y'' = floor (y' * real h)
        val intensity =
          if m >= max_mass then 0w255
          else Word8.fromInt (128 + floor ((m / max_mass) * 128.0))
        val color = Tigr.fromRgb (intensity, intensity, 0w255)
      in
        SOME ((x'', y''), color)
      end

  local val g = Random.newgen ()
  in
    fun rand () = Random.random g
    fun randi i = i * rand ()
  end

  fun mk_body_cloud (r: real) : body =
    let
      val a = rand () * 2.0 * Math.pi
      val z_a = rand () * 2.0 * Math.pi
      val l = rand () * r
      val pos = (Math.cos a * l, Math.sin a * l, Math.sin z_a * l)
    in
      {pos = pos, vel = V.zero, mass = rand ()}
    end

  type state =
    { ctx: Engine.ctx
    , bodies: Engine.Opaque.bodies.t
    , height: int
    , width: int
    , ul: real * real
    , br: real * real
    , invert: bool
    , max_mass: real
    , rotation: real * real
    , rotating: real * real
    , zoom: real
    , paused: bool
    , theta: real
    }

  fun withBodies
    ({ ctx
     , height
     , width
     , ul
     , br
     , invert
     , max_mass
     , rotation
     , rotating
     , zoom
     , paused
     , theta
     , ...
     }: state) (bodies: Engine.Opaque.bodies.t) : state =
    { ctx = ctx
    , height = height
    , width = width
    , ul = ul
    , br = br
    , invert = invert
    , max_mass = max_mass
    , rotation = rotation
    , rotating = rotating
    , zoom = zoom
    , paused = paused
    , theta = theta
    , bodies = bodies
    }

  fun withRotation
    ({ ctx
     , height
     , width
     , ul
     , br
     , invert
     , max_mass
     , rotating
     , zoom
     , paused
     , theta
     , bodies
     , ...
     }: state) r : state =
    { ctx = ctx
    , height = height
    , width = width
    , ul = ul
    , br = br
    , invert = invert
    , max_mass = max_mass
    , rotation = r
    , rotating = rotating
    , zoom = zoom
    , paused = paused
    , theta = theta
    , bodies = bodies
    }

  fun withRotating
    ({ ctx
     , height
     , width
     , ul
     , br
     , invert
     , max_mass
     , rotation
     , zoom
     , paused
     , theta
     , bodies
     , ...
     }: state) r : state =
    { ctx = ctx
    , height = height
    , width = width
    , ul = ul
    , br = br
    , invert = invert
    , max_mass = max_mass
    , rotation = rotation
    , rotating = r
    , zoom = zoom
    , paused = paused
    , theta = theta
    , bodies = bodies
    }

  fun withBrUl
    ({ ctx
     , height
     , width
     , invert
     , max_mass
     , rotation
     , rotating
     , zoom
     , paused
     , theta
     , bodies
     , ...
     }: state) br ul : state =
    { ctx = ctx
    , height = height
    , width = width
    , ul = ul
    , br = br
    , invert = invert
    , max_mass = max_mass
    , rotation = rotation
    , rotating = rotating
    , zoom = zoom
    , paused = paused
    , theta = theta
    , bodies = bodies
    }

  fun withWidthHeight
    ({ ctx = ctx
     , ul
     , br
     , invert
     , max_mass
     , rotation
     , rotating
     , zoom
     , paused
     , theta
     , bodies
     , ...
     }: state) width height : state =
    { ctx = ctx
    , height = height
    , width = width
    , ul = ul
    , br = br
    , invert = invert
    , max_mass = max_mass
    , rotation = rotation
    , rotating = rotating
    , zoom = zoom
    , paused = paused
    , theta = theta
    , bodies = bodies
    }

  fun withZoom
    ({ ctx
     , height
     , width
     , ul
     , br
     , invert
     , max_mass
     , rotation
     , rotating
     , paused
     , theta
     , bodies
     , ...
     }: state) z : state =
    { ctx = ctx
    , height = height
    , width = width
    , ul = ul
    , br = br
    , invert = invert
    , max_mass = max_mass
    , rotation = rotation
    , rotating = rotating
    , zoom = z
    , paused = paused
    , theta = theta
    , bodies = bodies
    }

  val eps = 50.0

  fun screen_to_world x y (s: state) =
    let
      val x_dist = #1 (#br s) - #1 (#ul s)
      val y_dist = #2 (#br s) - #2 (#ul s)
      val x = #1 (#ul s) + (real x / real (#width s)) * x_dist
      val y = #2 (#ul s) + (real y / real (#height s)) * y_dist
    in
      rotate_pos (inv_rotation (~(#1 (#rotation s))) (~(#2 (#rotation s))))
        (x, y, 0.0)
    end

  fun no_bodies ctx = Engine.Entry.no_bodies ctx ()

  fun bodies_to_monoarray (bodies: body list) =
    let
      val n = length bodies
      val arr = Real64Array.array (n * 7, 0.0)
      fun loop i [] = ()
        | loop i (b :: bs) =
            ( Real64Array.update (arr, i + 0, #mass b)
            ; Real64Array.update (arr, i + 1, #1 (#pos b))
            ; Real64Array.update (arr, i + 2, #2 (#pos b))
            ; Real64Array.update (arr, i + 3, #3 (#pos b))
            ; Real64Array.update (arr, i + 4, #1 (#vel b))
            ; Real64Array.update (arr, i + 5, #2 (#vel b))
            ; Real64Array.update (arr, i + 6, #3 (#vel b))
            ; loop (i + 7) bs
            )
    in
      loop 0 bodies;
      arr
    end

  fun add_bodies (ctx: Engine.ctx) (bodies: Engine.Opaque.bodies.t)
    (new_bodies: body list) =
    let
      val n = length new_bodies
      val arr =
        Engine.Real64Array2.new ctx
          (Real64ArraySlice.full (bodies_to_monoarray new_bodies)) (n, 7)
    in
      Engine.Entry.add_bodies ctx (bodies, arr)
      before Engine.Opaque.bodies.free bodies
      before Engine.Real64Array2.free arr
    end

  fun blob x y (s: state) =
    let
      val (x, y, z) = screen_to_world x y s
      val blobN = 20
      fun mv ({pos = (x0, y0, z0), vel, mass}: body) : body =
        {pos = (x0 + x, y0 + y, z0 + z), vel = vel, mass = mass}
      val bodies = List.tabulate (blobN, fn _ => mk_body_cloud 3.0)
    in
      withBodies s (add_bodies (#ctx s) (#bodies s) bodies)
    end

  fun init (ctx: Engine.ctx) (height: int) (width: int) : state =
    let
      val N = 1000
      val bodies = List.tabulate (N, fn _ =>
        mk_body_cloud (Real.min (real height, real width)))
      val max_mass = reduce Real.max 0.0 (map #mass bodies)
    in
      { ctx = ctx
      , bodies = add_bodies ctx (no_bodies ctx) bodies
      , height = height
      , width = width
      , ul = (real width / ~2.0, real height / ~2.0)
      , br = (real width / 2.0, real height / 2.0)
      , invert = false
      , max_mass = max_mass
      , rotation = (0.0, 0.0)
      , rotating = (0.0, 0.0)
      , zoom = 0.0
      , paused = false
      , theta = 0.5
      }
    end

  fun mv_bodies td (s: state) : state =
    withBodies s
      (if #paused s then
         #bodies s
       else
         Engine.Entry.step (#ctx s) (1, #bodies s)
         before Engine.Opaque.bodies.free (#bodies s))

  fun handl_rotation td (s: state) : state =
    withRotation s
      ( #1 (#rotation s) + td * #1 (#rotating s)
      , #2 (#rotation s) + td * #2 (#rotating s)
      )

  fun handl_zoom td (s: state) : state =
    let
      val x_dist = #1 (#br s) - #1 (#ul s)
      val y_dist = #2 (#br s) - #2 (#ul s)
    in
      withBrUl s
        (#1 (#br s) - td * x_dist * #zoom s, #2 (#br s) - td * y_dist * #zoom s)
        (#1 (#ul s) + td * x_dist * #zoom s, #2 (#ul s) + td * y_dist * #zoom s)
    end

  val op|| = Tigr.||
  infix ||
  val op&& = Tigr.&&
  infix &&

  infix |>
  fun v |> f = f v

  fun makeWindow (w: int, h: int, flags: Tigr.flags) : Tigr.tigr =
    Tigr.window {w = w, h = h, title = "NBody", flags = flags}

  fun drawWindow (win: Tigr.tigr) (s: state) : unit =
    let
      val h = Tigr.height win
      val w = Tigr.width win
      val s = withWidthHeight s w h
      val bodies_arr_fut = Engine.Entry.bodies_as_array (#ctx s) (#bodies s)
      val bodies_arr = Engine.Real64Array2.values bodies_arr_fut
      val () = Engine.Real64Array2.free bodies_arr_fut
      fun drawBody ({pos, vel, mass}: body) : unit =
        let
          val pos =
            rotate_pos (rotation (#1 (#rotation s)) (#2 (#rotation s))) pos
          val b = {vel = vel, pos = pos, mass = mass}
        in
          case render_body h w (#ul s) (#br s) (#max_mass s) b of
            NONE => ()
          | SOME ((x, y), c) =>
              let val r = floor (Math.sqrt (30.0 * #mass b))
              in Tigr.fillCircle {bmp = win, x = x, y = y, r = r, color = c}
              end
        end
      val n = Real64Array.length bodies_arr
      fun drawBodies i =
        if i = n then
          ()
        else
          let
            val b =
              { mass = Real64Array.sub (bodies_arr, i + 0)
              , pos =
                  ( Real64Array.sub (bodies_arr, i + 1)
                  , Real64Array.sub (bodies_arr, i + 2)
                  , Real64Array.sub (bodies_arr, i + 3)
                  )
              , vel =
                  ( Real64Array.sub (bodies_arr, i + 4)
                  , Real64Array.sub (bodies_arr, i + 5)
                  , Real64Array.sub (bodies_arr, i + 6)
                  )
              }
          in
            drawBody b;
            drawBodies (i + 7)
          end
    in
      drawBodies 0
    end

  fun step td s =
    s |> mv_bodies td |> handl_rotation td |> handl_zoom td

  fun main () =
    let
      val initialW = 800
      val initialH = 800
      val win = makeWindow (initialW, initialH, Tigr.TIGR_AUTO);
      val ctx = Engine.Context.new (Engine.Config.default)
      fun keyd k = Tigr.keyDown (win, k)
      fun key k = Tigr.keyHeld (win, k)
      fun loop (s: state) =
        if Tigr.closed win orelse keyd Tigr.TK_ESCAPE then
          (Tigr.free win; s)
        else
          let
            val () = Tigr.clear (win, Tigr.black)
            val s =
              if #height s <> Tigr.height win orelse #width s <> Tigr.width win then
                init ctx (Tigr.height win) (Tigr.width win)
              else
                s
            val () = drawWindow win s
            val s = withZoom s 0.0
            val s =
              if key Tigr.TK_RIGHT then
                withRotating s (#1 (#rotating s), 1.0)
              else if key Tigr.TK_LEFT then
                withRotating s (#1 (#rotating s), ~1.0)
              else if key Tigr.TK_UP then
                withRotating s (1.0, #2 (#rotating s))
              else if key Tigr.TK_DOWN then
                withRotating s (~1.0, #2 (#rotating s))
              else if key (Tigr.ascii #"Z") then
                withZoom s 1.0
              else if key (Tigr.ascii #"X") then
                withZoom s ~1.0
              else if key Tigr.TK_SPACE then
                withRotating s (0.0, 0.0)
              else if key (Tigr.ascii #"R") then
                init ctx (Tigr.height win) (Tigr.width win)
              else
                s
            val s = let val (x, y, b) = Tigr.mouse win
                    in if b > 0 then blob x y s else s
                    end
            val s = step 0.01 s
          in
            Tigr.update win;
            loop s
          end
    in
      loop (init ctx initialH initialW);
      Engine.Context.free ctx;
      ()
    end

  val () = main ()

end
