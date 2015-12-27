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
                  { src: 'images/checkerboard.jpg'
                  , size: new THREE.Vector2(50,50)
                  }
            )
            floor.position.set(25 * x_pt,25 * y_pt,0);
            point.add(floor)
        }
    }

/*
    var floor = surface(
          { src: 'images/checkerboard.jpg'
          , size: new THREE.Vector2(1000,1000)
          }
    )
    floor.position.x = 0.5
    floor.rotation.x = Math.PI / 2; // Rotations happend before the position is moved
//    scene.add(floor);

    // parent/pivot taken from http://jsfiddle.net/t83pzoj2/
	parent = new THREE.Object3D();
	scene.add( parent );

    // pivot
	pivot = new THREE.Object3D();
	parent.add( pivot );

    var floor2 = surface(
          { src: 'images/checkerboard.jpg'
          , size: new THREE.Vector2(100,100)
          }
    )

    floor2.position.y = -100.5;
    floor2.rotation.x = 0;
    floor2.rotation.z = 0.1;

    gui.folder('position',function() {
      this.add(floor2.position, 'y',-200,200).listen();
    })

//    scene.add(floor2);
    debug.floor2 = floor2

	floor.position.y = 100;
//    floor.scale.multiplyScalar( 4 );

	parent.add( floor );
	pivot.add( floor2 );



    ////////////
    // CUSTOM //
    ////////////

    var geometry = new THREE.SphereGeometry( 30, 32, 16 );
    var material = new THREE.MeshLambertMaterial( { color: 0x990088 } );
    mesh = new THREE.Mesh( geometry, material );
    mesh.position.set(0,40,0);
    scene.add(mesh);
*/
}

function world_update(scene,gui) {
    for(var i in scene.animate) {
//        console.log("i world_update",i)
        scene.animate[i].rotation.x += 0.005
        scene.animate[i].rotation.y += 0.005
    }
}
