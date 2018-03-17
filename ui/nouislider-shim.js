// A shim between Elm trying to interface with noUiSlider.js. It does't
// interface directly, but rather through this module.
//
// See https://guide.elm-lang.org/interop/javascript.html for more information.

function noUiSliderCreate(app) {
  // Handle Elm code calling 'NoUiSlider.create' port. See 'NoUiSlider.elm' for
  // the shape of the blob that we receive here.
  app.ports.noUiSliderCreate.subscribe(function(blob) {
    requestAnimationFrame(function() {
      // N.B. requestAnimationFrame is necessary to run this 'getElementById'
      // after the DOM is rendered (otherwise it would fail).
      var slider = document.getElementById(blob.id);

      noUiSlider.create(slider, {
        start: blob.start,

        // This is some quality code here. Translate Elm 'Nothing' values for
        // optional properties into 'undefined' manually.
        margin: (blob.margin ? blob.margin : undefined),
        limit: (blob.limit ? blob.limit : undefined),
        connect: (blob.connect ? blob.connect : undefined),
        direction: (blob.direction ? blob.direction : undefined),
        orientation: (blob.orientation ? blob.orientation : undefined),
        behavior: (blob.behavior ? blob.behavior : undefined),
        step: (blob.step ? blob.step : undefined),
        range: (blob.range ? blob.range : undefined),

        // I don't feel like doing these in Elm right now, so hard-code some stuff
        // here :)
        tooltips: [ wNumb({ decimals: 0 }), wNumb({ decimals: 0 }) ],
        pips: {
          mode: 'positions',
          values: [0, 25, 50, 75, 100],
          density: 5
        }
      });

      // When the slider values change, notify Elm via a port subscription called
      // 'noUiSliderOnUpdate'.
      slider.noUiSlider.on('update', function(values, handle, unencoded, tap, positions) {
        app.ports.noUiSliderOnUpdate.send(values.map(function(x) {
          return parseInt(x, 10)
        }));
      });
    });
  });
}
