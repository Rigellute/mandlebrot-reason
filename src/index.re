open Reprocessing;

let zoom = 200.;

let panX = 2.;

let panY = 1.5;

let maxIterations = 100;

let dimension = 600;

let square = (x: float) => x *. x;

let rec checkIfBelongsToMandelbrotSet = (x: float, y: float, iterations: int) => {
  let mandelColor =
    if (iterations > 0) {
      let realComponentOfResult = square(x) -. square(y) +. x;
      let imaginaryComponentOfResult = 2. *. x *. 2. *. y;
      let isInMandelbrot =
        realComponentOfResult *. imaginaryComponentOfResult > 5.;
      let color =
        if (isInMandelbrot) {
          float_of_int(maxIterations - iterations)
          /. float_of_int(maxIterations)
          *. 100.;
        } else {
          checkIfBelongsToMandelbrotSet(
            realComponentOfResult,
            imaginaryComponentOfResult,
            iterations - 1
          );
        };
      color;
    } else {
      0.;
    };
  mandelColor;
};

let setup = env => {
  Env.size(~width=dimension, ~height=dimension, env);
  for (x in 0 to dimension) {
    for (y in 0 to dimension) {
      let pixelColor =
        checkIfBelongsToMandelbrotSet(
          float_of_int(x) /. zoom -. panX,
          float_of_int(y) /. zoom -. panY,
          maxIterations
        );
      let _ =
        switch (pixelColor) {
        | 0. =>
          Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
          Draw.rect(~pos=(x, y), ~width=1, ~height=1, env);
        | _ =>
          Draw.fill(
            Utils.color(
              ~r=255,
              ~g=int_of_float(pixelColor) mod 255,
              ~b=int_of_float(pixelColor) mod 255,
              ~a=255
            ),
            env
          );
          Draw.rect(~pos=(x, y), ~width=1, ~height=1, env);
        };
      ();
    };
  };
  0;
};

run(~setup, ());