type vec = {x: f64, y: f64, z: f64}

def vecadd (a: vec) (b: vec) =
  {x = a.x+b.x, y = a.y+b.y, z = a.z+b.z}
def vecsub (a: vec) (b: vec) =
  {x = a.x-b.x, y = a.y-b.y, z = a.z-b.z}
def vecscale (s: f64) (a: vec) =
  {x = s * a.x, y = s * a.y, z = s * a.z}
def dot (a: vec) (b: vec) =
  a.x*b.x + a.y*b.y + a.z*b.z

type body = {pos: vec, vel: vec, mass: f64}

def EPSILON : f64 = 50

def accel (x: body) (y: body): vec =
  let r = y.pos `vecsub` x.pos
  let rsqr = dot r r + EPSILON
  let inv_dist = 1 / f64.sqrt rsqr
  let inv_dist3 = inv_dist * inv_dist * inv_dist
  let s = y.mass * inv_dist3
  in vecscale s r

def advance_body (dt: f64) (body: body) (acc: vec): body =
  body with pos = vecadd body.pos (vecscale dt body.vel)
       with vel = vecadd body.vel (vecscale dt acc)

def calc_accels [n] (bodies: [n]body): [n]vec =
  let move (body: body) =
    let accels = map (accel body) bodies
    in reduce_comm vecadd {x=0, y=0, z=0} accels
  in map move bodies

type~ bodies = []body

entry no_bodies : bodies = []

entry add_bodies (bs: bodies) (new: [][7]f64) : bodies =
  bs ++ map (\b -> {mass=b[0],
                    pos={x=b[1],y=b[2],z=b[3]},
                    vel={x=b[4],y=b[5],z=b[6]}}) new

entry step (dt: i32) (bodies: bodies) : bodies =
  map2 (advance_body (1/f64.i32 dt)) bodies
       (calc_accels bodies)

entry bodies_as_array (bs: bodies) : [][7]f64 =
  map (\b -> [b.mass, b.pos.x, b.pos.y, b.pos.z, b.vel.x, b.vel.y, b.vel.z]) bs
