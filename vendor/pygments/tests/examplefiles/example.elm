import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

-- Create a mesh with two triangles

type Vertex = { position:Vec3, color:Vec3 }

mesh : [Triangle Vertex]
mesh = [ ( Vertex (vec3 0  0 0) (vec3 1 0 0)
         , Vertex (vec3 1  1 0) (vec3 0 1 0)
         , Vertex (vec3 1 -1 0) (vec3 0 0 1)
         )
       ]

-- Create the scene

main : Signal Element
main = scene <~ foldp (+) 0 (fps 30)

scene : Float -> Element
scene t =
    webgl (400,400)
    [ entity vertexShader fragmentShader mesh { view = view (t / 1000) } ]

view : Float -> Mat4
view t =
    mul (makePerspective 45 1 0.01 100)
        (makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))

-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 } { unif | view:Mat4 } { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 view;
varying vec3 vcolor;

void main () {
    gl_Position = view * vec4(position, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
