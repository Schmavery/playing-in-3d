module Gl = Reasongl.Gl;
module Constants = RGLConstants;

let createShader = (context, source, kind) => {
  let shader = Gl.createShader(~context, kind);
  Gl.shaderSource(~context, ~shader, ~source);
  Gl.compileShader(~context, shader);
  let compileStatus =
    Gl.getShaderParameter(~context, ~shader, ~paramName=Gl.Compile_status);
  if (compileStatus != 1) {
    prerr_endline(Gl.getShaderInfoLog(~context, shader));
    raise(Failure("Could not compile shader"));
  };
  shader;
};

let createProgram =
    (context, {Shaders.attributes, uniforms, vertex, fragment}) => {
  let attributesSource = Shaders.generateVarList("attribute", attributes);
  let uniformsSource = Shaders.generateVarList("uniform", uniforms);
  let fullVertexSource =
    attributesSource ++ "\n" ++ uniformsSource ++ "\n" ++ vertex;
  let fullFragmentSource = uniformsSource ++ "\n" ++ fragment;
  Js.log(fullVertexSource);
  Js.log(fullFragmentSource);
  let vertexShader =
    createShader(context, fullVertexSource, Constants.vertex_shader);
  let fragmentShader =
    createShader(context, fullFragmentSource, Constants.fragment_shader);
  let program = Gl.createProgram(~context);
  Gl.attachShader(~context, ~program, ~shader=vertexShader);
  Gl.deleteShader(~context, vertexShader);
  Gl.attachShader(~context, ~program, ~shader=fragmentShader);
  Gl.deleteShader(~context, fragmentShader);
  Gl.linkProgram(~context, program);
  let linkStatus =
    Gl.getProgramParameter(~context, ~program, ~paramName=Gl.Link_status);
  if (linkStatus != 1) {
    prerr_endline(Gl.getProgramInfoLog(~context, program));
    raise(Failure("Could not link program"));
  };
  let attributeLocs =
    List.fold_left(
      (acc, v) =>
        Types.StringMap.add(
          Shaders.getVarName(v),
          Gl.getAttribLocation(
            ~context,
            ~program,
            ~name=Shaders.getVarName(v),
          ),
          acc,
        ),
      Types.StringMap.empty,
      attributes,
    );
  let uniformLocs =
    List.fold_left(
      (acc, v) =>
        Types.StringMap.add(
          Shaders.getVarName(v),
          Gl.getUniformLocation(
            ~context,
            ~program,
            ~name=Shaders.getVarName(v),
          ),
          acc,
        ),
      Types.StringMap.empty,
      uniforms,
    );
  {Shaders.attributeLocs, uniformLocs, program};
};

type keysT = {
  mutable left: bool,
  mutable right: bool,
  mutable up: bool,
  mutable down: bool,
  mutable space: bool,
};

let keys = {left: false, right: false, up: false, down: false, space: false};

let keyDown = (~keycode, ~repeat) =>
  switch ((keycode: Gl.Events.keycodeT)) {
  | A
  | Left => keys.left = true
  | D
  | Right => keys.right = true
  | W
  | Up => keys.up = true
  | S
  | Down => keys.down = true
  | Space => keys.space = true
  | _ => ()
  };

let keyUp = (~keycode) =>
  switch ((keycode: Gl.Events.keycodeT)) {
  | A
  | Left => keys.left = false
  | D
  | Right => keys.right = false
  | W
  | Up => keys.up = false
  | S
  | Down => keys.down = false
  | Space => keys.space = false
  | _ => ()
  };

let intersectRectCircle =
    (
      ~rectPos as (rx, ry),
      ~rectW,
      ~rectH,
      ~circlePos as (cx, cy),
      ~circleRad,
    ) => {
  let halfW = rectW /. 2.;
  let halfH = rectH /. 2.;
  let cdistX = abs_float(cx -. (rx +. halfW));
  let cdistY = abs_float(cy -. (ry +. halfH));
  if (cdistX > halfW +. circleRad || cdistY > halfH +. circleRad) {
    false;
  } else if (cdistX <= halfW || cdistY <= halfH) {
    true;
  } else {
    let cornerDistSq = (cdistX -. halfW) ** 2. +. (cdistY -. halfH) ** 2.;
    cornerDistSq <= circleRad ** 2.;
  };
};
let playerCollidesWithWall = (px, py, rad, wallSize, walls) =>
  List.exists(
    ((mapX, mapY)) =>
      intersectRectCircle(
        ~rectPos=(
          float_of_int(mapX) *. wallSize,
          float_of_int(mapY) *. wallSize,
        ),
        ~rectW=wallSize,
        ~rectH=wallSize,
        ~circlePos=(px, py),
        ~circleRad=rad,
      ),
    walls,
  );
let tryToMove = (moveAmount, pos, walls, maze: Maze.t) => {
  let [|x, _, y|] = Patch.Vec3f.(pos + moveAmount);
  if (int_of_float(x) < maze.width
      && x >= 0.
      && int_of_float(y) < maze.height
      && y >= 0.
      && !playerCollidesWithWall(x, y, 1.0, 1.0, walls)) {
    Patch.Vec3f.add(pos, pos, moveAmount);
  };
};
let getWalls = (maze: Maze.t) => {
  let walls = ref([]);
  for (y in 0 to maze.height - 1) {
    for (x in 0 to maze.width - 1) {
      /* for (x in 0 to 1) { */
      if (maze.map[x + y * maze.width] == 1) {
        walls := [(x, y), ...walls^];
      };
    };
  };
  walls^;
};
let walls = getWalls(Maze.mazeDef);

let moveSpeed = 30.;
let turnSpeed = 1.;
let pos = [|4., 1.75, 6.|];
let look = [|0., 0., 0.1|];

let first = ref(true);
let time = ref(0.0);

let drawScene =
    (window, context, projectionMatrix, program, lightingProgram, models, dt) => {
  Gl.clear(
    ~context,
    ~mask=Constants.color_buffer_bit lor Constants.depth_buffer_bit,
  );

  if (keys.space) {
    time := time^ +. dt;
  };
  if (first^) {
    Js.log("walls:");
    Js.log(Array.of_list(walls));
  };
  if (keys.up) {
    let moveAmount = Patch.Vec3f.make();
    Patch.Vec3f.scale(moveAmount, look, -. dt *. moveSpeed *. 2.);
    let [|x, _, y|] = moveAmount;
    tryToMove([|x, 0., 0.|], pos, walls, Maze.mazeDef);
    tryToMove([|0., 0., y|], pos, walls, Maze.mazeDef);
  };
  if (keys.down) {
    let moveAmount = Patch.Vec3f.make();
    Patch.Vec3f.scale(moveAmount, look, dt *. moveSpeed);
    let [|x, _, y|] = moveAmount;
    tryToMove([|x, 0., 0.|], pos, walls, Maze.mazeDef);
    tryToMove([|0., 0., y|], pos, walls, Maze.mazeDef);
  };
  if (keys.left) {
    let rotate = Patch.Mat3f.createYRotation(dt *. turnSpeed);
    Patch.Mat3f.matvecmul(rotate, look);
  };
  if (keys.right) {
    let rotate = Patch.Mat3f.createYRotation(-. dt *. turnSpeed);
    Patch.Mat3f.matvecmul(rotate, look);
  };

  let viewMatrix = Patch.Mat4f.make();
  Patch.Mat4f.lookAt(
    viewMatrix,
    Patch.Vec3f.(look + pos),
    pos,
    [|0., 1., 0.|],
  );
  let viewMatrix = Patch.Mat4f.toGlMat4(viewMatrix);

  let modelMatrix = Gl.Mat4.create();
  if (first^) {
    Js.log(context);
    first := false;
  };

  let drawGuy = (x, z) => {
    Gl.Mat4.identity(~out=modelMatrix);
    Gl.Mat4.translate(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~vec=[|x, 1.3, z|],
    );
    Gl.Mat4.rotate(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~rad=Patch.pi /. 2.,
      ~vec=[|1., 0., 0.|],
    );
    let grey = 1.0;
    let s = 0.35;
    Gl.Mat4.scale(~out=modelMatrix, ~matrix=modelMatrix, ~vec=[|s, s, s|]);
    Draw.drawModel(
      projectionMatrix,
      modelMatrix,
      viewMatrix,
      pos,
      (grey, grey, grey, grey),
      context,
      lightingProgram,
      Types.StringMap.find("guy", models),
    );
  };

  let drawWithOffset = (x, y, z, scale, model, grey) => {
    Gl.Mat4.identity(~out=modelMatrix);
    Gl.Mat4.rotate(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~rad=0.00001,
      ~vec=[|0., 1., 0.|],
    );
    Gl.Mat4.translate(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~vec=[|x, y, z|],
    );
    Gl.Mat4.scale(~out=modelMatrix, ~matrix=modelMatrix, ~vec=scale);
    Draw.drawModel(
      projectionMatrix,
      modelMatrix,
      viewMatrix,
      pos,
      (grey, grey, grey, grey),
      context,
      lightingProgram,
      model,
    );
  };

  let wallScale = [|1.0, 3.0, 1.0|];
  let floorScale = [|40.0, 1.0, 40.0|];

  let cube = Types.StringMap.find("cube", models);
  Gl.useProgram(~context, lightingProgram.program);
  for (y in 0 to Maze.mazeDef.height - 1) {
    for (x in 0 to Maze.mazeDef.width - 1) {
      if (Maze.mazeDef.map[x + y * Maze.mazeDef.width] == 1) {
        drawWithOffset(
          float_of_int(x),
          0.0,
          float_of_int(y),
          wallScale,
          cube,
          1.0,
        );
      };
    };
  };
  drawWithOffset(0.0, -1.0, 0.0, floorScale, cube, 0.3);
  drawWithOffset(0.0, 3.0, 0.0, floorScale, cube, 0.3);

  drawGuy(5.0, 3.0);

  Gl.Mat4.identity(~out=modelMatrix);
  Gl.Mat4.identity(~out=viewMatrix);
  Gl.Mat4.translate(
    ~out=modelMatrix,
    ~matrix=modelMatrix,
    ~vec=[|0., (-0.5), (-1.3)|],
  );
  let scale = 0.3;
  Gl.Mat4.scale(
    ~out=modelMatrix,
    ~matrix=modelMatrix,
    ~vec=[|scale, scale, scale|],
  );
  Gl.Mat4.rotate(
    ~out=modelMatrix,
    ~matrix=modelMatrix,
    ~rad=-1.3,
    ~vec=[|1., 0., 0.|],
  );
  Gl.Mat4.rotate(
    ~out=modelMatrix,
    ~matrix=modelMatrix,
    ~rad=time^ *. 8.,
    ~vec=[|0., 1., 0.|],
  );
  Gl.useProgram(~context, program.Shaders.program);
  Draw.drawModel(
    projectionMatrix,
    modelMatrix,
    viewMatrix,
    pos,
    (1.0, 1.0, 1.0, 1.0),
    context,
    program,
    Types.StringMap.find("gun", models),
  );
  if (keys.space) {
    Gl.Mat4.identity(~out=modelMatrix);
    Gl.Mat4.identity(~out=viewMatrix);
    Gl.Mat4.translate(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~vec=[|(-0.04), (-0.3), (-1.6)|],
    );
    let scale = 0.05 *. (sin(time^ *. 50.0) +. 1.0);
    Gl.Mat4.scale(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~vec=[|scale, scale, scale|],
    );
    Gl.Mat4.rotate(
      ~out=modelMatrix,
      ~matrix=modelMatrix,
      ~rad=1.0,
      ~vec=[|1., 0., 0.|],
    );
    Gl.useProgram(~context, program.Shaders.program);
    let alpha = sin(time^) *. 1.0;
    Draw.drawModel(
      projectionMatrix,
      modelMatrix,
      viewMatrix,
      pos,
      (4.0, 2.0, 2.0, alpha),
      context,
      program,
      Types.StringMap.find("explode", models),
    );
  };
};

let window = Gl.Window.init(~screen="main", ~argv=Sys.argv);
let windowW = 640;
let windowH = 480;
Gl.Window.setWindowSize(~window, ~width=windowW, ~height=windowH);
let context = Gl.Window.getContext(window);
Gl.enable(~context, Constants.depth_test);
Gl.enable(~context, Patch.cull_face);
let program = createProgram(context, Shaders.default);
let lightingProgram = createProgram(context, Shaders.distanceLit);
let w = Gl.Window.getWidth(window);
let h = Gl.Window.getHeight(window);
let projectionMatrix = Gl.Mat4.create();

Patch.perspective(
  ~out=projectionMatrix,
  ~fovy=45. *. Patch.pi /. 180.,
  ~aspect=float_of_int(w) /. float_of_int(h),
  ~near=0.1,
  ~far=100.0,
);

Draw.loadModels(
  [
    ("cube", "./models/texcube.obj", "./textures/wall_pot.png"),
    ("gun", "./models/gun.obj", "./textures/guncolors.png"),
    ("explode", "./models/explode.obj", "./textures/explosion.png"),
    ("guy", "./models/guy.obj", "./textures/guy.png"),
  ],
  context,
  models => {
    let displayFunc = dt =>
      drawScene(
        window,
        context,
        projectionMatrix,
        program,
        lightingProgram,
        models,
        dt /. 1000.,
      );
    /* TODO Why is this ignored */
    ignore @@ Gl.render(~window, ~displayFunc, ~keyDown, ~keyUp, ());
  },
);
