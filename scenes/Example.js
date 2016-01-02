var Example = { };



Example.init = function (t) {
    var scene = t.scene
    var gui   = t.gui

    // LIGHT
    var light = new THREE.PointLight(0xffffff);
    light.position.set(100,250,100);
    scene.add(light);
    // FLOOR

    var floor = t.card(
          { src: 'images/checkerboard.jpg'
          , size: new THREE.Vector2(512,512)
          }
    )
    floor.position.x = 0.5
    floor.rotation.x = Math.PI / 2; // Rotations happend before the position is moved
    t.add(floor);

    ////////////
    // CUSTOM //
    ////////////

    var geometry = new THREE.SphereGeometry( 30, 32, 16 );
    var material = new THREE.MeshLambertMaterial( { color: 0x990088 } );
    mesh = new THREE.Mesh( geometry, material );
    mesh.position.set(0,40,0);
    t.add(mesh);
}
