[@bs.scope "mat4"] [@bs.module "gl-matrix"]
external perspective:
  (
    ~out: Reasongl.Gl.Mat4.t,
    ~fovy: float,
    ~aspect: float,
    ~near: float,
    ~far: float
  ) =>
  unit =
  "";

let pi = 4.0 *. atan(1.0);

let cull_face = 2884;

let rec split = (str, pointer, sep, accstr, acc) =>
  if (pointer >= String.length(str)) {
    List.rev([accstr, ...acc]);
  } else {
    let ch = str.[pointer];
    if (ch == sep) {
      split(str, pointer + 1, sep, "", [accstr, ...acc]);
    } else {
      split(str, pointer + 1, sep, accstr ++ String.make(1, ch), acc);
    };
  };

let split = (str, ~sep) => split(str, 0, sep, "", []);

module Vec3f = {
  type t = array(float);
  let make = () => [|0., 0., 0.|];
  let scale = (out, v, s) => {
    out[0] = v[0] *. s;
    out[1] = v[1] *. s;
    out[2] = v[2] *. s;
  };
  let copyTo = (out, v) => {
    out[0] = v[0];
    out[1] = v[1];
    out[2] = v[2];
  };
  let (+) = (v1, v2) => [|v1[0] +. v2[0], v1[1] +. v2[1], v1[2] +. v2[2]|];
  let add = (out, v1, v2) => {
    out[0] = v1[0] +. v2[0];
    out[1] = v1[1] +. v2[1];
    out[2] = v1[2] +. v2[2];
  };
};

module Mat4f = {
  type t = array(float);
  let toJsMat4 = m: Reasongl.Gl.Mat4.t => Obj.magic(m);
  let make = (): t => Array.make(16, 0.);
  let identity = out => {
    out[0] = 1.;
    out[1] = 0.;
    out[2] = 0.;
    out[3] = 0.;
    out[4] = 0.;
    out[5] = 1.;
    out[6] = 0.;
    out[7] = 0.;
    out[8] = 0.;
    out[9] = 0.;
    out[10] = 1.;
    out[11] = 0.;
    out[12] = 0.;
    out[13] = 0.;
    out[14] = 0.;
    out[15] = 1.;
  };
  let lookAt =
      (
        out,
        [|eyex, eyey, eyez|],
        [|centerx, centery, centerz|],
        [|upx, upy, upz|],
      ) => {
    let epsilon = 0.000001;
    if (abs_float(eyex -. centerx) < epsilon
        && abs_float(eyey -. centery) < epsilon
        && abs_float(eyez -. centerz) < epsilon) {
      identity(out);
    } else {
      let z0 = eyex -. centerx;
      let z1 = eyey -. centery;
      let z2 = eyez -. centerz;
      let len = 1. /. sqrt(z0 *. z0 +. z1 *. z1 +. z2 *. z2);
      let z0 = z0 *. len;
      let z1 = z1 *. len;
      let z2 = z2 *. len;
      let x0 = upy *. z2 -. upz *. z1;
      let x1 = upz *. z0 -. upx *. z2;
      let x2 = upx *. z1 -. upy *. z0;
      let len = sqrt(x0 *. x0 +. x1 *. x1 +. x2 *. x2);
      let (len, x0, x1, x2) =
        if (len == 0.) {
          (len, 0., 0., 0.);
        } else {
          let len = 1. /. len;
          (len, x0 *. len, x1 *. len, x2 *. len);
        };
      let y0 = z1 *. x2 -. z2 *. x1;
      let y1 = z2 *. x0 -. z0 *. x2;
      let y2 = z0 *. x1 -. z1 *. x0;
      let len = sqrt(y0 *. y0 +. y1 *. y1 +. y2 *. y2);
      let (len, y0, y1, y2) =
        if (len == 0.) {
          (len, 0., 0., 0.);
        } else {
          let len = 1. /. len;
          (len, y0 *. len, y1 *. len, y2 *. len);
        };
      out[0] = x0;
      out[1] = y0;
      out[2] = z0;
      out[3] = 0.;
      out[4] = x1;
      out[5] = y1;
      out[6] = z1;
      out[7] = 0.;
      out[8] = x2;
      out[9] = y2;
      out[10] = z2;
      out[11] = 0.;
      out[12] = -. (x0 *. eyex +. x1 *. eyey +. x2 *. eyez);
      out[13] = -. (y0 *. eyex +. y1 *. eyey +. y2 *. eyez);
      out[14] = -. (z0 *. eyex +. z1 *. eyey +. z2 *. eyez);
      out[15] = 1.;
    };
  };
};

module Mat3f = {
  let createIdentity = () => [|1., 0., 0., 0., 1., 0., 0., 0., 1.|];
  let createZRotation = theta => [|
    cos(theta),
    -. sin(theta),
    0.,
    sin(theta),
    cos(theta),
    0.,
    0.,
    0.,
    1.,
  |];

  let createYRotation = theta => [|
    cos(theta),
    0.,
    sin(theta),
    0.,
    1.,
    0.,
    -. sin(theta),
    0.,
    cos(theta),
  |];

  let createXRotation = theta => [|
    1.,
    0.,
    0.,
    0.,
    cos(theta),
    -. sin(theta),
    0.,
    sin(theta),
    cos(theta),
  |];

  /***
   [0 1 2]   [a]   [a0 + b1 + c2]
   [3 4 5] * [b] = [a3 + b4 + c5]
   [6 7 8]   [c]   [a6 + b7 + c8]
   */
  let matvecmul = (m: array(float), v: array(float)) => {
    let a = v[0];
    let b = v[1];
    let c = v[2];
    v[0] = a *. m[0] +. b *. m[1] +. c *. m[2];
    v[1] = a *. m[3] +. b *. m[4] +. c *. m[5];
    v[2] = a *. m[6] +. b *. m[7] +. c *. m[8];
  };
};
