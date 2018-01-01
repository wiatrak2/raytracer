all: ray

ray:
	ocamlc -o ray vec.ml ray.ml camera.ml sphere.ml trace.ml

clean:
	rm *.c* ray

.PHONY: clean ray