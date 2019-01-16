type glslVarType =
  | Vec4(string)
  | Vec3(string)
  | Vec2(string)
  | Sampler2D(string)
  | Mat4(string);

type shaderDefT = {
  attributes: list(glslVarType),
  uniforms: list(glslVarType),
  vertex: string,
  fragment: string,
};

type programT = {
  uniformLocs: Types.StringMap.t(Reasongl.Gl.uniformT),
  attributeLocs: Types.StringMap.t(Reasongl.Gl.attributeT),
  program: Reasongl.Gl.programT,
};

let generateVarList = (kind, l) =>
  String.concat(
    "\n",
    List.map(
      var =>
        switch (var) {
        | Vec4(s) => kind ++ " vec4 " ++ s ++ ";"
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
  | Vec4(s)
  | Vec3(s)
  | Vec2(s)
  | Sampler2D(s)
  | Mat4(s) => s
  };

let default = {
  attributes: [
    Vec4("aVertexPosition"),
    Vec3("aVertexNormal"),
    Vec2("aVertexUV"),
  ],
  uniforms: [
    Mat4("uModelMatrix"),
    Mat4("uViewMatrix"),
    Mat4("uProjectionMatrix"),
    Vec3("uPlayerPos"),
    Vec4("uTintColor"),
    Sampler2D("uSampler"),
  ],
  vertex: {|
  varying vec4 vNormal;
  varying vec2 vUV;

  void main(void) {
    gl_Position = uProjectionMatrix * uViewMatrix * uModelMatrix * aVertexPosition;
    vNormal = normalize(uViewMatrix * uModelMatrix * vec4(aVertexNormal, 0.0));
    vUV = aVertexUV;
  }
|},
  fragment: {|
  varying vec4 vNormal;
  varying vec2 vUV;

  void main(void) {
    vec4 s = texture2D(uSampler, vUV);
    vec4 ambient = vec4(0.2, 0.2, 0.2, 0.0);
    vec4 lightDirection = normalize(vec4(1.0, -1.0, 0.0, 0.0));
    /* float l = clamp(-dot(vNormal, lightDirection), 0.0, 1.0); */
    float l = clamp(-dot(vNormal, lightDirection), 0.0, 1.0);
    /* float l = length(dot(vNormal, normalize(lightDirection))); */
    gl_FragColor = s * vec4(l, l, l, 1.) + ambient * uTintColor;
    /* gl_FragColor = s; */
  }
|},
};

let distanceLit = {
  attributes: [
    Vec4("aVertexPosition"),
    Vec3("aVertexNormal"),
    Vec2("aVertexUV"),
  ],
  uniforms: [
    Mat4("uModelMatrix"),
    Mat4("uViewMatrix"),
    Mat4("uProjectionMatrix"),
    Vec3("uPlayerPos"),
    Vec4("uTintColor"),
    Sampler2D("uSampler"),
  ],
  vertex: {|
  varying vec4 vNormal;
  varying vec2 vUV;
  varying vec4 world_Position;

  void main(void) {
    world_Position = uModelMatrix * aVertexPosition;
    gl_Position = uProjectionMatrix * uViewMatrix * uModelMatrix * aVertexPosition;
    vNormal = uModelMatrix * vec4(aVertexNormal, 0.0);
    /* vPlayerDist = 2.0 / distance(world_Position, vec4(uPlayerPos, 0.0)); */
    vUV = aVertexUV;
  }
|},
  fragment: {|
  varying vec4 vNormal;
  varying vec2 vUV;
  varying vec4 world_Position;

  void main(void) {
    vec4 s = texture2D(uSampler, vUV);
    /* float l = clamp(-dot(vNormal.xyz, normalize(lightDirection)), 0.0, 1.0); */
    /* vec4 ambient = vec4(0.2, 0.2, 0.2, 0.0); */
    /* float l = length(dot(vNormal.xyz, normalize(uLightDirection))); */
    /* gl_FragColor = s * vec4(l, l, l, l) + ambient; */
    /* float playerDist = min(2.0 / distance(world_Position, vec4(uPlayerPos, 0.0)) + 0.2, 1.0); */
    float playerDist = smoothstep(0.0, 1.0, 0.2 + (2.0 / distance(world_Position, vec4(uPlayerPos, 0.0))));
    gl_FragColor = s * uTintColor * vec4(playerDist, playerDist, playerDist, 1.0);
  }
|},
};
