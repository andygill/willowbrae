function world(scene, gui) {
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

            var point = new THREE.Object3D();
            point.position.set(scale * x_pt,scale * y_pt,0);
            scene.add(point);
            scene.animate.push(point)

            var anchor = new THREE.SphereGeometry( 5, 16, 8 );
            var material = new THREE.MeshBasicMaterial( { color: 0x000000, wireframe: true } );
            mesh = new THREE.Mesh( anchor, material );
            point.add(mesh);

            var floor = surface(
                { // src:  'images/checkerboard.jpg' 
                    // From https://en.wikipedia.org/wiki/Portable_Network_Graphics
                    src: 'images/PNG_transparency_demonstration_1.png'
                , size: new THREE.Vector2(80,60)
                }
            )
            floor.position.set(80 * 0.5 * x_pt,60 * 0.5 * y_pt,0);
            point.add(floor)
        }
    }

}

function world_update(scene,gui) {
    for(var i in scene.animate) {
//        console.log("i world_update",i)
        scene.animate[i].rotation.x += 0.005
        scene.animate[i].rotation.y += 0.005
    }
}
