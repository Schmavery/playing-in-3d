let vsSource = {|
  attribute vec4 aVertexPosition;
  attribute vec3 aVertexNormal;
  attribute vec2 aVertexUV;

  uniform mat4 uModelViewMatrix;
  uniform mat4 uProjectionMatrix;
  uniform vec3 uLightDirection;
  uniform vec3 uTintColor;

  varying vec4 vNormal;
  varying vec2 vUV;

  void main(void) {
    gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
    vNormal = uModelViewMatrix * vec4(aVertexNormal, 0.0);
    vUV = aVertexUV;
  }
|};

let fsSource = {|
  uniform vec3 uLightDirection;
  uniform mat4 uModelViewMatrix;
  uniform sampler2D uSampler;
  uniform vec3 uTintColor;

  varying vec4 vNormal;
  varying vec2 vUV;

  void main(void) {
    vec4 s = texture2D(uSampler, vUV);
    float l = length(dot(vNormal.xyz, normalize(uLightDirection)));
    gl_FragColor = s * vec4(uTintColor, 1.0) * vec4(l, l, l, l);
  }
|};
