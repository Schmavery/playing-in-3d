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

let createProgram = context => {
  let vertexShader = createShader(context, Shaders.vsSource, Constants.vertex_shader);
  let fragmentShader =
    createShader(context, Shaders.fsSource, Constants.fragment_shader);
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
  program;
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

let initBuffers = context => {
  let positionBuffer =
    loadBuffer(context, Cube.positions, Float32, Constants.array_buffer);
  /* let colorBuffer = */
  /*   loadBuffer(context, Cube.colors, Float32, Constants.array_buffer); */
  let colorBuffer = Gl.createBuffer(~context);
  let normalBuffer =
    loadBuffer(context, Cube.normals, Float32, Constants.array_buffer);
  let uvBuffer =
    loadBuffer(context, Cube.uvs, Float32, Constants.array_buffer);
  let indexBuffer =
    loadBuffer(context, Cube.indexes, Uint16, Constants.element_array_buffer);
  {
    Types.positionBuffer,
    colorBuffer,
    normalBuffer,
    indexBuffer,
    uvBuffer,
    numIndexes: Cube.numIndexes,
  };
};

/* TODO: Move the getAttribLocation stuff */
let configureAttribs =
    (
      context,
      program,
      {positionBuffer, colorBuffer, normalBuffer, indexBuffer, uvBuffer}: Types.buffers,
    ) => {
  Gl.bindBuffer(
    ~context,
    ~target=Constants.array_buffer,
    ~buffer=positionBuffer,
  );
  let aVertexPosition =
    Gl.getAttribLocation(~context, ~program, ~name="aVertexPosition");
  Gl.vertexAttribPointer(
    ~context,
    ~attribute=aVertexPosition,
    ~size=3,
    ~type_=Constants.float_,
    ~normalize=false,
    ~stride=0,
    ~offset=0,
  );
  Gl.enableVertexAttribArray(~context, ~attribute=aVertexPosition);

  /* Gl.bindBuffer( */
  /*   ~context, */
  /*   ~target=Constants.array_buffer, */
  /*   ~buffer=colorBuffer, */
  /* ); */
  /* let aVertexColor = */
  /*   Gl.getAttribLocation(~context, ~program, ~name="aVertexColor"); */
  /* Gl.vertexAttribPointer( */
  /*   ~context, */
  /*   ~attribute=aVertexColor, */
  /*   ~size=4, */
  /*   ~type_=Constants.float_, */
  /*   ~normalize=false, */
  /*   ~stride=0, */
  /*   ~offset=0, */
  /* ); */
  /* Gl.enableVertexAttribArray(~context, ~attribute=aVertexColor); */

  Gl.bindBuffer(~context, ~target=Constants.array_buffer, ~buffer=uvBuffer);
  let aVertexUV = Gl.getAttribLocation(~context, ~program, ~name="aVertexUV");
  Gl.vertexAttribPointer(
    ~context,
    ~attribute=aVertexUV,
    ~size=2,
    ~type_=Constants.float_,
    ~normalize=false,
    ~stride=0,
    ~offset=0,
  );
  Gl.enableVertexAttribArray(~context, ~attribute=aVertexUV);

  Gl.bindBuffer(
    ~context,
    ~target=Constants.array_buffer,
    ~buffer=normalBuffer,
  );
  let aVertexNormal =
    Gl.getAttribLocation(~context, ~program, ~name="aVertexNormal");
  Gl.vertexAttribPointer(
    ~context,
    ~attribute=aVertexNormal,
    ~size=3,
    ~type_=Constants.float_,
    ~normalize=false,
    ~stride=0,
    ~offset=0,
  );
  Gl.enableVertexAttribArray(~context, ~attribute=aVertexNormal);

  Gl.bindBuffer(
    ~context,
    ~target=Constants.element_array_buffer,
    ~buffer=indexBuffer,
  );
};

let totalTime = ref(0.0);

let drawScene = (window, context, program, buffers, dt) => {
  totalTime := totalTime^ +. dt;
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

  let modelViewMatrix = Gl.Mat4.create();
  Gl.Mat4.translate(
    ~out=modelViewMatrix,
    ~matrix=modelViewMatrix,
    ~vec=[|0.0, 0.0, (-6.0)|],
  );

  Gl.Mat4.rotate(
    ~out=modelViewMatrix,
    ~matrix=modelViewMatrix,
    ~rad=totalTime^,
    ~vec=[|0., 1., 0.|],
  );
  Gl.Mat4.rotate(
    ~out=modelViewMatrix,
    ~matrix=modelViewMatrix,
    ~rad=totalTime^ /. 2.,
    ~vec=[|1., 0., 0.|],
  );

  configureAttribs(context, program, buffers);

  let uProjectionMatrix =
    Gl.getUniformLocation(~context, ~program, ~name="uProjectionMatrix");
  Gl.uniformMatrix4fv(
    ~context,
    ~location=uProjectionMatrix,
    ~value=projectionMatrix,
  );

  let uLightDirection =
    Gl.getUniformLocation(~context, ~program, ~name="uLightDirection");
  Gl.uniform3f(~context, ~location=uLightDirection, ~v1=0., ~v2=1., ~v3=1.);

  let uLightDirection =
    Gl.getUniformLocation(~context, ~program, ~name="uLightDirection");
  Gl.uniform3f(~context, ~location=uLightDirection, ~v1=0., ~v2=1., ~v3=1.);

  let uTintColor =
    Gl.getUniformLocation(~context, ~program, ~name="uTintColor");
  Gl.uniform3f(~context, ~location=uTintColor, ~v1=1.0, ~v2=0.0, ~v3=0.0);

  let uModelViewMatrix =
    Gl.getUniformLocation(~context, ~program, ~name="uModelViewMatrix");
  Gl.uniformMatrix4fv(
    ~context,
    ~location=uModelViewMatrix,
    ~value=modelViewMatrix,
  );

  let uSampler = Gl.getUniformLocation(~context, ~program, ~name="uSampler");
  Gl.uniform1i(~context, ~location=uSampler, ~value=0);

  Gl.drawElements(
    ~context,
    ~mode=Constants.triangles,
    ~count=buffers.numIndexes,
    ~type_=Constants.unsigned_short,
    ~offset=0,
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
ObjLoader.loadModel("./models/monkey.obj", bufferArrays =>
/* ObjLoader.loadModel("./models/monkeysmooth.obj", bufferArrays => */
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
          let windowW = 640;
          let windowH = 480;
          Gl.Window.setWindowSize(~window, ~width=windowW, ~height=windowH);

          let context = Gl.Window.getContext(window);

          setupImage(context, img);

          Gl.enable(~context, Constants.depth_test);
          /* The example didn't use viewport for some reason */
          /* Gl.viewport(~context, ~x=0, ~y=0, ~width=windowW, ~height=windowH); */
          let program = createProgram(context);
          Gl.useProgram(~context, program);
          /* let buffers = initBuffers(context); */
          let buffers = ObjLoader.initModelBuffers(context, bufferArrays);
          let displayFunc = dt =>
            drawScene(window, context, program, buffers, dt /. 1000.);
          /* TODO Why is this ignored */
          ignore @@ Gl.render(~window, ~displayFunc, ());
        },
    (),
  )
);
