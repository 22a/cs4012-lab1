# SVGoodness

A web service that lets you describe drawings in a custom shape DSL and have them rendered as SVG in realtime in front of your very eyes.


## Prerequisites

* [Haskell Stack](https://www.haskellstack.org)


## Installation

Clone the repo:
```bash
git clone https://github.com/22a/cs4012-lab1.git && cd cs4012-lab1
```

Setup the project:
```bash
stack setup
```

Build the project:
```bash
stack build
```

Run the server:
```bash
stack exec cs4012-lab1-exe
```

## Usage

Navitage to `localhost:3000` in your browser


## Shape DSL Syntax

A `Drawing` is a list of `Shapes`.
A `Shape` is a triple of a basic shape, the stylesheet for that shape, and a transformation to be applied to that shape.
Transformations are composed with the infix `:>` operator and are applied from left to right.


### Possible Basic Shapes
* Empty
* Square
* Circle

### Stylesheets
Stylesheets are a triple of stroke width, stroke colour, and fill colour. All three are mandatory.

### Possible Transformations
* Ident
* Translate tx ty
* Scale sx sy
* SkewX angle
* SkewY angle
* Rotate angle

The order of transformation matters, these two are not the same:
* `(Scale 2.0 2.0)       :> (Translate 10.0 10.0)`
* `(Translate 10.0 10.0) :> (Scale 2.0 2.0)`
