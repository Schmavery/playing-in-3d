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

let generateVarList = (kind, l) =>
  String.concat(
    "\n",
    List.map(
      var =>
        switch (var) {
        | Shaders.Vec4(s) => kind ++ " vec4 " ++ s ++ ";"
        | Vec3(s) => kind ++ " vec3 " ++ s ++ ";"
        | Vec2(s) => kind ++ " vec2 " ++ s ++ ";"
        | Sampler2D(s) => kind ++ " sampler2D " ++ s ++ ";"
        | Mat4(s) => kind ++ " mat4 " ++ s ++ ";"
        },
      l,
    ),
  );

let getVarName = var =>
  switch (var) {
  | Shaders.Vec4(s)
  | Vec3(s)
  | Vec2(s)
  | Sampler2D(s)
  | Mat4(s) => s
  };

let createProgram =
    (context, {Shaders.attributes, uniforms, vertex, fragment}) => {
  let attributesSource = generateVarList("attribute", attributes);
  let uniformsSource = generateVarList("uniform", uniforms);
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
          getVarName(v),
          Gl.getAttribLocation(~context, ~program, ~name=getVarName(v)),
          acc,
        ),
      Types.StringMap.empty,
      attributes,
    );
  let uniformLocs =
    List.fold_left(
      (acc, v) =>
        Types.StringMap.add(
          getVarName(v),
          Gl.getUniformLocation(~context, ~program, ~name=getVarName(v)),
          acc,
        ),
      Types.StringMap.empty,
      uniforms,
    );
  {Shaders.attributeLocs, uniformLocs, program};
};

let loadBuffer = (context, data, datatype, target) => {
  let buffer = Gl.createBuffer(~context);
  Gl.bindBuffer(~context, ~target, ~buffer);
  Gl.bufferData(
    ~context,
    ~target,
    ~data=Gl.Bigarray.(of_array(datatype, data)),
    ~usage=Constants.static_draw,
  );
  buffer;
};

let initBuffers = (context, buffers: Types.bufferArrays) => {
  let positionBuffer =
    loadBuffer(context, buffers.positions, Float32, Constants.array_buffer);
  let normalBuffer =
    loadBuffer(context, buffers.normals, Float32, Constants.array_buffer);
  let uvBuffer =
    loadBuffer(context, buffers.uvs, Float32, Constants.array_buffer);
  let indexBuffer =
    loadBuffer(
      context,
      buffers.indexes,
      Uint16,
      Constants.element_array_buffer,
    );
  {
    Types.positionBuffer,
    normalBuffer,
    indexBuffer,
    uvBuffer,
    numIndexes: Array.length(buffers.indexes),
  };
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

let moveSpeed = 30.;
let turnSpeed = 1.;
let pos = [|4., 1.75, 6.|];
let look = [|0., 0., 0.1|];

let first = ref(true);
let time = ref(0.0);

let drawScene = (window, context, program, buffers, gun, dt) => {
  Gl.clear(
    ~context,
    ~mask=Constants.color_buffer_bit lor Constants.depth_buffer_bit,
  );

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

  if (keys.space) {
    time := time^ +. dt;
  };
  if (keys.up) {
    let moveAmount = Patch.Vec3f.make();
    Patch.Vec3f.scale(moveAmount, look, -. dt *. moveSpeed);

    let [|x, _, y|] = Patch.Vec3f.(pos + moveAmount);
    let newX = int_of_float(x);
    let newY = int_of_float(y);
    if (newX
        + newY
        * Maze.mazeDef.width < Array.length(Maze.mazeDef.map)
        && Maze.mazeDef.map[newX + newY * Maze.mazeDef.width] == 0) {
      Patch.Vec3f.add(pos, pos, moveAmount);
    };
  };
  if (keys.down) {
    let moveAmount = Patch.Vec3f.make();
    Patch.Vec3f.scale(moveAmount, look, dt *. moveSpeed);
    let [|x, _, y|] = Patch.Vec3f.(pos + moveAmount);
    let newX = int_of_float(x);
    let newY = int_of_float(y);
    if (newX
        + newY
        * Maze.mazeDef.width < Array.length(Maze.mazeDef.map)
        && Maze.mazeDef.map[newX + newY * Maze.mazeDef.width] == 0) {
      Patch.Vec3f.add(pos, pos, moveAmount);
    };
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
  let viewMatrix = Patch.Mat4f.toJsMat4(viewMatrix);

  let modelMatrix = Gl.Mat4.create();
  if (first^) {
    Js.log(context);
    first := false;
  };

  let drawWithOffset = (x, y, z, scale, grey) => {
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
    Draw.drawBuffer(
      projectionMatrix,
      modelMatrix,
      viewMatrix,
      pos,
      grey,
      context,
      program,
      buffers,
    );
  };

  let wallScale = [|1.0, 3.0, 1.0|];
  let floorScale = [|40.0, 1.0, 40.0|];

  for (y in 0 to Maze.mazeDef.height - 1) {
    for (x in 0 to Maze.mazeDef.width - 1) {
      if (Maze.mazeDef.map[x + y * Maze.mazeDef.width] == 1) {
        drawWithOffset(
          float_of_int(x),
          0.0,
          float_of_int(y),
          wallScale,
          1.0,
        );
      };
    };
  };
  drawWithOffset(0.0, -1.0, 0.0, floorScale, 0.3);
  drawWithOffset(0.0, 3.0, 0.0, floorScale, 0.3);

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
  Draw.drawBuffer(
    projectionMatrix,
    modelMatrix,
    viewMatrix,
    pos,
    2.3,
    context,
    program,
    gun,
  );
};

let setupImage = (context, image) => {
  let filter = Constants.linear;
  let texture = Gl.createTexture(~context);
  Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture);
  Gl.texImage2DWithImage(
    ~context,
    ~target=Constants.texture_2d,
    ~level=0,
    ~image,
  );
  Gl.texParameteri(
    ~context,
    ~target=Constants.texture_2d,
    ~pname=Constants.texture_mag_filter,
    ~param=filter,
  );
  Gl.texParameteri(
    ~context,
    ~target=Constants.texture_2d,
    ~pname=Constants.texture_min_filter,
    ~param=filter,
  );
};

/* ObjLoader.loadModel("./models/texcube.obj", bufferArrays => */
/* ObjLoader.loadModel("./models/sphere.obj", bufferArrays => */
/* ObjLoader.loadModel("./models/spheresmooth.obj", bufferArrays => */
ObjLoader.loadModel("./models/gun.obj", bufferArrays
  /* ObjLoader.loadModel("./models/monkeysmooth.obj", bufferArrays => */
  =>
    Gl.loadImage(
      ~filename="./textures/wall_pot.png",
      /* ~filename="./textures/apple.png", */
      ~loadOption=LoadRGBA,
      ~callback=
        imageData =>
          switch (imageData) {
          | None => failwith("Could not load image")
          | Some(img) =>
            let window = Gl.Window.init(~screen="main", ~argv=Sys.argv);
            Js.log(window);
            let windowW = 640;
            let windowH = 480;
            Gl.Window.setWindowSize(~window, ~width=windowW, ~height=windowH);

            let context = Gl.Window.getContext(window);

            setupImage(context, img);

            Gl.enable(~context, Constants.depth_test);
            Gl.enable(~context, Patch.cull_face);
            /* The example didn't use viewport for some reason */
            /* Gl.viewport(~context, ~x=0, ~y=0, ~width=windowW, ~height=windowH); */
            let program = createProgram(context, Shaders.default);
            Gl.useProgram(~context, program.program);
            /* let buffers = initBuffers( */
            /*   context, */
            /*   Draw.createCubeBuffers(0., 0., 0., 1., 2.)); */
            let buffers = initBuffers(context, Cube.buffers);
            let gun = initBuffers(context, bufferArrays);
            let displayFunc = dt =>
              drawScene(window, context, program, buffers, gun, dt /. 1000.);
            /* TODO Why is this ignored */
            ignore @@ Gl.render(~window, ~displayFunc, ~keyDown, ~keyUp, ());
          },
      (),
    )
  );
