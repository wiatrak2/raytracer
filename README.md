OCaml Ray Tracer
=======================

Functional Programming 2017/18

Wojciech Pratkowiecki

University of Wroclaw

-----------------

An inplementation of ray tracer for given scene. Input is a simple JSON file parsed with **Yojson** package. To get Yojson use [opam](https://opam.ocaml.org) package manager, you can get Yojson run ``opam install yojson`` command. Some features of the ray tracer:

 - Two object types: spheres and planes
 - Lambert shading
 - Phong shading
 - Mirror reflection
 - Two light source types: uniform and point lighting
 - camera situated in given position with given direction
 - possible gamma correction

The repository contains an example input file ``input.json``. To compile the program use ``make`` command. To execute generated ``raytracer`` binary provide input file name as argument e.g. ``./raytracer input.json`` will execute the program with exemplary input.

The output is **.ppm** file, for instance:

![output](https://github.com/wiatrak2/raytracer/blob/master/output.jpg?raw=true)

---------------
