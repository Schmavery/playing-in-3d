let fos = float_of_string;
let ios = int_of_string;

type vertex = {
  pos: (float, float, float),
  uv: (float, float),
  normal: (float, float, float),
};

module VertexMap =
  Map.Make({
    type t = vertex;
    let compare = compare;
  });

type objData = {
  vertex: list((float, float, float)),
  vertexNormal: list((float, float, float)),
  vertexTexture: list((float, float)),
};

let getIndexData = (data, str) => {
  let positionData = Array.of_list(data.vertex);
  let normalData = Array.of_list(data.vertexNormal);
  let textureData = Array.of_list(data.vertexTexture);

  let addVertex = (vertexList, indexCount, indexList, indexMap, pi, ti, ni) => {
    let newVertex = {
      pos: positionData[pi - 1],
      uv: textureData[ti - 1],
      normal: normalData[ni - 1],
    };
    switch (VertexMap.find(newVertex, indexMap)) {
    | index => (vertexList, indexCount, [index, ...indexList], indexMap)
    | exception Not_found => (
        [newVertex, ...vertexList],
        indexCount + 1,
        [indexCount, ...indexList],
        VertexMap.add(newVertex, indexCount, indexMap),
      )
    };
  };

  let (vl, iCount, il, iMap) =
    List.fold_left(
      ((vl, iCount, il, iMap), line) =>
        switch (Patch.split(line, ~sep=' ')) {
        | ["f", v1, v2, v3] =>
          let split1 = Patch.split(v1, ~sep='/');
          let split2 = Patch.split(v2, ~sep='/');
          let split3 = Patch.split(v3, ~sep='/');
          switch (split1, split2, split3) {
          | ([vi1, uv1, ni1], [vi2, uv2, ni2], [vi3, uv3, ni3]) =>
            let (vl, iCount, il, iMap) =
              addVertex(
                vl,
                iCount,
                il,
                iMap,
                ios(vi1),
                ios(uv1),
                ios(ni1),
              );
            let (vl, iCount, il, iMap) =
              addVertex(
                vl,
                iCount,
                il,
                iMap,
                ios(vi2),
                ios(uv2),
                ios(ni2),
              );
            let result =
              addVertex(
                vl,
                iCount,
                il,
                iMap,
                ios(vi3),
                ios(uv3),
                ios(ni3),
              );
            result;
          | _ => failwith("Cannot parse face")
          };
        | _ => (vl, iCount, il, iMap)
        },
      ([], 0, [], VertexMap.empty),
      Patch.split(str, ~sep='\n'),
    );
  (vl, iCount, il, iMap);
};

let getVertexData = str => {
  let data =
    List.fold_left(
      (data, line) =>
        switch (Patch.split(line, ~sep=' ')) {
        | ["v", x, y, z] => {
            ...data,
            vertex: [(fos(x), fos(y), fos(z)), ...data.vertex],
          }
        | ["vn", x, y, z] => {
            ...data,
            vertexNormal: [
              (fos(x), fos(y), fos(z)),
              ...data.vertexNormal,
            ],
          }
        | ["vt", x, y] => {
            ...data,
            vertexTexture: [(fos(x), fos(y)), ...data.vertexTexture],
          }
        | _ => data
        },
      {vertex: [], vertexNormal: [], vertexTexture: []},
      Patch.split(str, ~sep='\n'),
    );
  {
    vertex: List.rev(data.vertex),
    vertexNormal: List.rev(data.vertexNormal),
    vertexTexture: List.rev(data.vertexTexture),
  };
};

let convertToBuffers = vertexList => {
  let bufferSize = List.length(vertexList);
  let posBuff = Array.make(bufferSize * 3, 0.0);
  let normBuff = Array.make(bufferSize * 3, 0.0);
  let uvBuff = Array.make(bufferSize * 2, 0.0);
  List.iteri(
    (i, v) => {
      let (px, py, pz) = v.pos;
      posBuff[i * 3] = px;
      posBuff[i * 3 + 1] = py;
      posBuff[i * 3 + 2] = pz;
      let (nx, ny, nz) = v.normal;
      normBuff[i * 3] = nx;
      normBuff[i * 3 + 1] = ny;
      normBuff[i * 3 + 2] = nz;
      let (uvx, uvy) = v.uv;
      uvBuff[i * 2] = uvx;
      uvBuff[i * 2 + 1] = uvy;
    },
    vertexList,
  );
  (posBuff, normBuff, uvBuff);
};

let loadModel = (filename, cb) => {
  Reasongl.Gl.File.readFile(
    ~filename,
    ~cb=str => {
      let data = getVertexData(str);
      let (vertexList, ic, indexList, _) = getIndexData(data, str);
      Printf.printf(
        "#verts: %d, #indexes: %d\n",
        List.length(vertexList),
        List.length(indexList),
      );
      let (positions, normals, uvs) =
        convertToBuffers(List.rev(vertexList));
      let indexes = Array.of_list(List.rev(indexList));
      cb({Types.positions, normals, uvs, indexes});
    },
  );
  ();
};

let loadBuffer = (context, data, datatype, target) => {
  let buffer = Reasongl.Gl.createBuffer(~context);
  Reasongl.Gl.bindBuffer(~context, ~target, ~buffer);
  Reasongl.Gl.bufferData(
    ~context,
    ~target,
    ~data=Reasongl.Gl.Bigarray.(of_array(datatype, data)),
    ~usage=RGLConstants.static_draw,
  );
  buffer;
};

let initModelBuffers = (context, buffers: Types.bufferArrays): Types.buffers => {
  let positionBuffer =
    loadBuffer(
      context,
      buffers.positions,
      Float32,
      RGLConstants.array_buffer,
    );
  /* TODO remove color or something */
  let colorBuffer = Reasongl.Gl.createBuffer(~context);
  let normalBuffer =
    loadBuffer(context, buffers.normals, Float32, RGLConstants.array_buffer);
  let uvBuffer =
    loadBuffer(context, buffers.uvs, Float32, RGLConstants.array_buffer);
  let indexBuffer =
    loadBuffer(
      context,
      buffers.indexes,
      Uint16,
      RGLConstants.element_array_buffer,
    );
  let numIndexes = Array.length(buffers.indexes);
  {
    positionBuffer,
    colorBuffer,
    normalBuffer,
    indexBuffer,
    uvBuffer,
    numIndexes,
  };
};
