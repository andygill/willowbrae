var Example = { };

debug = {}

Example.init = function (t) {
    var scene = t.scene
    var gui   = t.gui

    // LIGHT
    var light = new THREE.PointLight(0xffffff);
    light.position.set(100,250,100);
    scene.add(light);
    // FLOOR

    t.gui.folder('light',function() {
      this.add(light.position, 'x').min(-256).max(256).step(16)
      this.add(light.position, 'y').min(-256).max(256).step(16)
      this.add(light.position, 'z').min(-256).max(256).step(16)
      return true
    })


/*
    var floor = t.card(
      { src: 'images/rustytiles01_diff.jpg'
      , specular: 'images/rustytiles01_spec.jpg'
      , size: new THREE.Vector2(512,512)
      , repeat: new THREE.Vector2(4,4)
      }
    )
*/
    var floor = t.card(
      { material: THREE.MeshPhongMaterial
      , src:      'images/woodfloor_c.jpg'
      , specular: 'images/woodfloor_s_z.png'
      , normal:   'images/woodfloor_n.png'
      , size: new THREE.Vector2(512,512)
      }
    )


//    floor.rotation.z = Math.PI / 2; // Rotations happend before the position is moved
    floor.rotation.x = -Math.PI / 2; // Rotations happend before the position is moved
    t.add(floor);

    var back_wall = t.card(
          { material: THREE.MeshPhongMaterial
          , color: new THREE.Color("#123456") //src: 'images/checkerboard.jpg'
          , size: new THREE.Vector2(512,256)
          , anchor: new THREE.Vector2(512 / 2, 256)
          }
    )
    back_wall.position.z -= 256
//    back_wall.card.material.wireframe = true
//     back_wall.card.material.color = new THREE.Color("#ff0000")
//    back_wall.card.material.vertexColors = THREE.FaceColors
    back_wall.card.material.needsUpdate = true // This is needed
    debug.floor = back_wall
    t.add(back_wall);

    var left_wall = t.card(
          { src: 'images/green_wall.jpeg'
          , size: new THREE.Vector2(512,256)
          , anchor: new THREE.Vector2(512 / 2, 256)
          , repeat: new THREE.Vector2(4,4)
          }
    )
    left_wall.rotation.y = Math.PI / 2; // Rotations happend before the position is moved
    console.log("position",left_wall.position)
    left_wall.position.x = -256
    debug.position = left_wall.position
    t.add(left_wall)
    t.debug(left_wall,"Left Wall")

    var right_wall = t.card(
        // From https://en.wikipedia.org/wiki/Portable_Network_Graphics
        { src: 'images/PNG_transparency_demonstration_1.png'
        , size: new THREE.Vector2(512,256)
        , anchor: new THREE.Vector2(512 / 2,256)
        , transparent: true
        }
    )
    right_wall.rotation.y = -Math.PI / 2; // Rotations happend before the position is moved
    right_wall.position = new THREE.Vector3(0,0,0)
    t.add(right_wall);



    t.gui.folder('walks',function() {
      this.addColor(back_wall.card.material,'color')
      this.add(left_wall.position, 'x').min(-256).max(256).step(16)
      this.add(right_wall.position, 'x').min(-256).max(256).step(16)
      return true
    })


    ////////////
    // CUSTOM //
    ////////////

    var geometry = new THREE.SphereGeometry( 30, 32, 16 );
    var material = new THREE.MeshLambertMaterial( { color: 0x990088 } );
    mesh = new THREE.Mesh( geometry, material );
    mesh.position.set(0,40,0);
    t.add(mesh);
}
