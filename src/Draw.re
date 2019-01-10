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

let drawModel =
    (
      projectionMatrix,
      modelMatrix,
      viewMatrix,
      [|playerx, playery, playerz|],
      grey,
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
  Gl.uniform4f(
    ~context,
    ~location=uTintColor,
    ~v1=grey,
    ~v2=grey,
    ~v3=grey,
    ~v4=grey,
  );

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
