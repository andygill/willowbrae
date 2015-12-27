function world(scene, gui) {
    // LIGHT
    var light = new THREE.PointLight(0xffffff);
    light.position.set(100,250,100);
    scene.add(light);
    // FLOOR

    var floor = surface(
          { src: 'images/checkerboard.jpg'
          , size: new THREE.Vector2(1000,1000)
          }
    )
    floor.position.x = 0.5
    floor.rotation.x = Math.PI / 2; // Rotations happend before the position is moved
    scene.add(floor);

    var floor = surface(
          { src: 'images/checkerboard.jpg'
          , size: new THREE.Vector2(100,100)
          }
    )

    floor.position.y = -100.5;
    floor.rotation.x = 0;
    floor.rotation.z = 0.1;

    gui.folder('position',function() {
      this.add(floor.position, 'y',-200,200)
    })

    scene.add(floor);
    debug.floor_ = floor

    ////////////
    // CUSTOM //
    ////////////

    var geometry = new THREE.SphereGeometry( 30, 32, 16 );
    var material = new THREE.MeshLambertMaterial( { color: 0x990088 } );
    mesh = new THREE.Mesh( geometry, material );
    mesh.position.set(0,40,0);
    scene.add(mesh);
}

function world_update(scene,gui) {
}
