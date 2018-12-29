let positions = ModelData.positions;
let normals =
  Array.of_list(
    List.fold_right(
      ([x, y, z], l) => [
        x, y, z,
        x, y, z,
        x, y, z,
        x, y, z,
        ...l,
      ],
      ModelData.faceNormals,
      [],
    ),
  );
let colors =
  Array.of_list(
    List.fold_right(
      ([r, g, b, a], l) => [
        r, g, b, a,
        r, g, b, a,
        r, g, b, a,
        r, g, b, a,
        ...l,
      ],
      ModelData.faceColors,
      [],
    ),
  );
let indexes = ModelData.indexes;

let uvs = ModelData.uvs;

let numIndexes = 36;
