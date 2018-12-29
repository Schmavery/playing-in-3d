[@bs.scope "mat4"] [@bs.module "gl-matrix"]
external perspective:
  (~out: Reasongl.Gl.Mat4.t, ~fovy: float, ~aspect: float, ~near: float, ~far: float) =>
  unit =
  "";

let pi = 4.0 *. atan(1.0);

let rec split = (str, pointer, sep, accstr, acc) =>
  if (pointer >= String.length(str)){
    List.rev([accstr, ...acc])
  } else {
    let ch = String.get(str, pointer);
    if (ch == sep) {
      split(str, pointer + 1, sep, "", [accstr, ...acc])
    } else {
      split(str, pointer + 1, sep, accstr ++ String.make(1, ch), acc)
    }
  }

let split = (str, ~sep) => split(str, 0, sep, "", []);
