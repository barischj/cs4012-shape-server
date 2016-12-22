# Shape server

The existing Shapes language is in `Shapes.hs`. Though very little remains, only`Vector`, `Shape` and `Transform`. A `Style` data type was introduced which represents some common styling attributes applied to SVG elements, such as fill colour and width. Colours are represented by the introduced data type `Rgb` which implements `Read` and `Show`, reading values in the format #rrggbb and showing them in the same format. Finally a `Drawing` type was introduced which combines multiple shapes, each with a transform and multiple styles.

`Svg.hs` contains functions for converting from the Shapes language to valid SVG. `shapeToSvg` provides a basic conversion from `Shape` to `Svg`. Then the functions `styleSvg` and `transformSvg` can be used to apply a `Style` or `Transform`. If multiple `Transforms` are provided via the `Compose` value constructor then they are `mappend`ed together, since `AttributeValue` is an instance of `Monoid`. `Svg`s are also instances of `Monoid` which `drawingToSvg` makes use of to combine multiple shapes.

`Server.hs` exports a Scotty server which provides a Shape language to SVG conversion service.

## Usage

`stack build && stack exec shape-server-exe`

Then visit `localhost:3000` which provides a web form for entering an SVG description and displaying the result.

Alternatively send a POST request to `/svg` with the SVG description in the body.
