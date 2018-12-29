type bufferArrays = {
  positions: array(float),
  normals: array(float),
  uvs: array(float),
  indexes: array(int),
};

type buffers = {
  positionBuffer: Reasongl.Gl.bufferT,
  colorBuffer: Reasongl.Gl.bufferT,
  normalBuffer: Reasongl.Gl.bufferT,
  indexBuffer: Reasongl.Gl.bufferT,
  uvBuffer: Reasongl.Gl.bufferT,
  numIndexes: int,
};
