let pi = 4.0 *. atan(1.0);

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
