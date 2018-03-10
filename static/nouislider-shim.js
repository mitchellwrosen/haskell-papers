// A shim between Elm trying to interface with noUiSlider.js. It does't
// interface directly, but rather through this module.
//
// See https://guide.elm-lang.org/interop/javascript.html for more information.

function noUiSliderCreate(app) {
  // Handle Elm code calling 'NoUiSlider.create' port. See 'NoUiSlider.elm' for
  // the shape of the blob that we receive here.
  app.ports.noUiSliderCreate.subscribe(function(blob) {
    var slider = document.getElementById(blob.id);

    noUiSlider.create(slider, {
      start: blob.start,

      // This is some code here. Translate Elm 'Nothing' values for optional
      // properties into 'undefined' manually.
      margin: (blob.margin ? blob.margin : undefined),
      limit: (blob.limit ? blob.limit : undefined),
      connect: (blob.connect ? blob.connect : undefined),
      direction: (blob.direction ? blob.direction : undefined),
      orientation: (blob.orientation ? blob.orientation : undefined),
      behavior: (blob.behavior ? blob.behavior : undefined),
      step: (blob.step ? blob.step : undefined),
      tooltips: (blob.tooltips ? blob.tooltips : undefined),
      range: (blob.range ? blob.range : undefined),
      pipes: (blob.pipes ? blob.pipe : undefined)
    });
  });
}
