type t = {
  width: int,
  height: int,
  map: array(int),
};

let mazeStr = {|
1111111111111111111111111111111
1000000000000100000000000000001
1000000000000100000000000000001
1000000000000100000000000000001
1000000000000100000000000000001
1000000000000100000000000000001
1000000000000100000000000000001
1000000000000000000000001111111
1000000000000000000000001000001
1000000000000000000000001000001
1000000000000000000000001000001
1000000000000000001000001000001
1000000000000000001000001000001
1111111000000000001000001000001
1000000000000000001111111000001
1000000000000000000000000000001
1000000000000000000000000000001
1000000000000000000000000000001
1000000000010000010000000000001
1000000000010000010000000000001
1000000000010000010000000000001
1000000000011111110000000000001
1000000000000000000000000000001
1000000000000000000000000000001
1000000000000000000000000000001
1111111111111111111111111111111|};

let mazeDef = {
  let strlist = List.tl(Patch.split(mazeStr, ~sep='\n'));
  let width = String.length(List.hd(strlist));
  let height = List.length(strlist);
  let fullStr = String.concat("", strlist);
  let map = Array.make(width * height, 0);
  String.iteri(
    (i, c) =>
      switch (c) {
      | '0' => map[i] = 0
      | _ => map[i] = 1
      },
    fullStr,
  );
  {width, height, map};
};

type vert = {
  p: (float, float, float),
  n: (float, float, float),
  uv: (float, float),
};

let generateMesh = (mazeDef, mazeSize, wallHeight, tileSize) => {
  /* TODO: */
  /* - Generate cube for each 1 in map */
  /* - Generate floor and ceiling mesh for each 0 in map */
  /* - Collapse common verts and calculate indices */
  let verts = ref([]);

  let add = v => verts := [v, ...verts^];

  for (x in 0 to mazeDef.width) {
    for (y in 0 to mazeDef.height) {
      /* Add cube to arrays */
      let h = wallHeight;
      let s = float_of_int(tileSize);
      let xo = float_of_int(x) *. s;
      let yo = float_of_int(y) *. s;
      /* +x +y +z */
      add({p: (1. +. xo, 0. +. yo, 0.), n: (1., 1., 0.), uv: (0., 0.)});
    };
  };
};
