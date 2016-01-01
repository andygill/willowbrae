var ANCHOR = { };

ANCHOR.init = function (t) {
    var scene = t.scene
    var gui   = t.gui

    // LIGHT
    var light = new THREE.PointLight(0xffffff);
    light.position.set(100,250,100);
    scene.add(light);

    // FLOOR
    var rows = columns = 3;
    var scale = 100;
    t.animate2 = [];

    for(var x = 0; x < columns; x++) {
        for(var y = 0; y < rows; y++) {

            var x_pt = x - (columns - 1) / 2
            var y_pt = y - (rows - 1) / 2

            var floor = t.card(
                // From https://en.wikipedia.org/wiki/Portable_Network_Graphics
                { src: 'images/PNG_transparency_demonstration_1.png'
                , size: new THREE.Vector2(80,60)
                , anchor: new THREE.Vector2(80 * x / (columns - 1), 60 * y / (rows - 1))
                }
            )
            floor.position.set(scale * x_pt,scale * y_pt,0);
            floor.update = function () {
              this.rotation.x += 0.005
              this.rotation.y += 0.005
            }
            t.add(floor)
        }
    }
};

