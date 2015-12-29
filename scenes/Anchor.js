X.main = function () {
    var scene = this.scene
    var gui   = this.gui

    // LIGHT
    var light = new THREE.PointLight(0xffffff);
    light.position.set(100,250,100);
    scene.add(light);
    // FLOOR

    var rows = columns = 3;
    var scale = 100;
    scene.animate = [];

    for(var x = 0; x < columns; x++) {
        for(var y = 0; y < rows; y++) {

            var x_pt = x - (columns - 1) / 2
            var y_pt = y - (rows - 1) / 2

            var floor = this.card(
                { // src:  'images/checkerboard.jpg' 
                    // From https://en.wikipedia.org/wiki/Portable_Network_Graphics
                    src: 'images/PNG_transparency_demonstration_1.png'
                , size: new THREE.Vector2(80,60)
                    , anchor: new THREE.Vector2(80 * x / (columns - 1), 60 * y / (rows - 1))
                }
            )
            floor.position.set(scale * x_pt,scale * y_pt,0);
            scene.add(floor)
            scene.animate.push(floor)
        }
    }
}

// Need to figure out something better than this
X.update2 = X.update;
X.update = function() {
    X.update2()
    for(var i in X.scene.animate) {
//        console.log("i world_update",i)
        X.scene.animate[i].rotation.x += 0.005
        X.scene.animate[i].rotation.y += 0.005
    }
}
