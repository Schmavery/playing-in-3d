module Gl = Reasongl.Gl;
module Constants = RGLConstants;

let empty = {Types.positions: [||], normals: [||], uvs: [||], indexes: [||]};

let createCubeBuffers = (xOff, yOff, zOff, size, height, buffers) => {
  let positions =
    Array.map(
      ({x, y, z}: Types.Vec3.t) => (
        {x: x *. size +. xOff, y: y *. size +. yOff, z: z *. height +. zOff}: Types.Vec3.t
      ),
      Cube.positions,
    );
  {
    ...Cube.buffers,
    Types.positions:
      Array.of_list(
        Array.fold_right(
          ({x, y, z}: Types.Vec3.t, l) => [x, y, z, ...l],
          positions,
          [],
        ),
      ),
  };
};

let setupTexture = (context, image) => {
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
  texture;
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

let initBuffers = (context, buffers: Types.bufferArrays, texture) => {
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
    texture,
    numIndexes: Array.length(buffers.indexes),
  };
};

let loadModels = (names: list((string, string, string)), context, cb) => {
  open Types;
  let numModels = List.length(names);
  let map = ref(StringMap.empty);
  List.map(
    ((name, model, image)) =>
      ObjLoader.loadModel(model, bufferArrays =>
        Gl.loadImage(
          ~filename=image,
          ~loadOption=LoadRGBA,
          ~callback=
            imageData =>
              switch (imageData) {
              | None => failwith("Could not load image")
              | Some(img) =>
                let texture = setupTexture(context, img);
                let buffers = initBuffers(context, bufferArrays, texture);
                map := StringMap.add(name, buffers, map^);
                if (StringMap.cardinal(map^) == numModels) {
                  cb(map^);
                };
              },
          (),
        )
      ),
    names,
  );
};

let drawModel =
    (
      projectionMatrix,
      modelMatrix,
      viewMatrix,
      [|playerx, playery, playerz|],
      (r, g, b, a),
      context,
      program: Shaders.programT,
      {
        positionBuffer,
        normalBuffer,
        uvBuffer,
        indexBuffer,
        texture,
        numIndexes,
      }: Types.buffers,
    ) => {
  open Types;
  Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture);
  Gl.bindBuffer(
    ~context,
    ~target=Constants.array_buffer,
    ~buffer=positionBuffer,
  );
  let aVertexPos = StringMap.find("aVertexPosition", program.attributeLocs);
  Gl.vertexAttribPointer(
    ~context,
    ~attribute=aVertexPos,
    ~size=3,
    ~type_=Constants.float_,
    ~normalize=false,
    ~stride=0,
    ~offset=0,
  );
  Gl.enableVertexAttribArray(~context, ~attribute=aVertexPos);

  Gl.bindBuffer(~context, ~target=Constants.array_buffer, ~buffer=uvBuffer);
  let aVertexUV = StringMap.find("aVertexUV", program.attributeLocs);
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
  let aVertexNormal = StringMap.find("aVertexNormal", program.attributeLocs);
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

  let uProjectionMatrix =
    StringMap.find("uProjectionMatrix", program.uniformLocs);
  Gl.uniformMatrix4fv(
    ~context,
    ~location=uProjectionMatrix,
    ~value=projectionMatrix,
  );

  let uPlayerPos = StringMap.find("uPlayerPos", program.uniformLocs);
  Gl.uniform3f(
    ~context,
    ~location=uPlayerPos,
    ~v1=playerx,
    ~v2=playery,
    ~v3=playerz,
  );

  let uTintColor = StringMap.find("uTintColor", program.uniformLocs);
  Gl.uniform4f(~context, ~location=uTintColor, ~v1=r, ~v2=g, ~v3=b, ~v4=a);

  let uModelMatrix = StringMap.find("uModelMatrix", program.uniformLocs);
  Gl.uniformMatrix4fv(~context, ~location=uModelMatrix, ~value=modelMatrix);

  let uViewMatrix = StringMap.find("uViewMatrix", program.uniformLocs);
  Gl.uniformMatrix4fv(~context, ~location=uViewMatrix, ~value=viewMatrix);

  /* Draws using texture 0 */
  let uSampler = StringMap.find("uSampler", program.uniformLocs);
  Gl.uniform1i(~context, ~location=uSampler, ~value=0);

  Gl.drawElements(
    ~context,
    ~mode=Constants.triangles,
    ~count=numIndexes,
    ~type_=Constants.unsigned_short,
    ~offset=0,
  );
};
