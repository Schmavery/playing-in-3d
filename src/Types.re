module StringMap = Map.Make(String);

type bufferArrays = {
  positions: array(float),
  normals: array(float),
  uvs: array(float),
  indexes: array(int),
};

type buffers = {
  positionBuffer: Reasongl.Gl.bufferT,
  normalBuffer: Reasongl.Gl.bufferT,
  indexBuffer: Reasongl.Gl.bufferT,
  uvBuffer: Reasongl.Gl.bufferT,
  texture: Reasongl.Gl.textureT,
  numIndexes: int,
};

module Vec3 = {
  type t = {
    x: float,
    y: float,
    z: float,
  };
};

module Vec2 = {
  type t = {
    x: float,
    y: float,
  };
};
