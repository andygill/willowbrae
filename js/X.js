// Wrapper for the THREE.js library

/*
	Template originally taken from Three.js "tutorials by example" by Lee Stemkoski,
  though it has been though many changes now.
 */

(function () {

  X = {}

  // library-global
  var keyboard = X.keyboard = new KeyboardState();
  var clock = X.keyboard = new THREE.Clock();

  X.init = function () {


    var gui = X.gui = new GUI();
        
  	// SCENE
  	var scene = X.scene = new THREE.Scene();

  	// CAMERA
  	var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
  	var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 20000;

  	camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);
  	scene.add(camera);
  	camera.position.set(0,150,400);
  	camera.lookAt(scene.position);	

  	if ( Detector.webgl )
  		renderer = X.renderer = new THREE.WebGLRenderer( {antialias:false, alpha: true } );
  	else
  		renderer = X.renderer = new THREE.CanvasRenderer(); 
  	renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
  	container = document.getElementById( 'ThreeJS' );
  	container.appendChild( renderer.domElement );
    // EVENTS
    THREEx.WindowResize(renderer, camera);
  //  THREEx.FullScreen.bindKey({ charCode : 'm'.charCodeAt(0) });
  	// CONTROLS
  	controls = new THREE.OrbitControls( camera, renderer.domElement );
  	// STATS

  	stats = new Stats();
  	stats.domElement.style.position = 'absolute';
  	stats.domElement.style.bottom = '0px';
  	stats.domElement.style.zIndex = 100;
  	container.appendChild( stats.domElement );

    X.main()
    // The controller


    X.gui.folder("Camera",function() {
      this.add(window, 'fps', { Slow: 1, Slower: 10, "Full Speed": null } );
      this.add(camera, 'fov').listen().onChange(function(value) {
        camera.updateProjectionMatrix()
      })
      this.folder('position',function() {
        this.add(camera.position, 'x').listen();
        this.add(camera.position, 'y').listen();
        this.add(camera.position, 'z').listen();
      })
      return true;
    })    

  }


  X.render = function () 
  {
  	X.renderer.render( scene, camera );
  }

  X.update = function()
  {
  	if ( keyboard.pressed("z") ) 
  	{	  
      console.log("z pressed")
  		// do something
  	}
	
  	controls.update();
  	stats.update();
  }



  // Should be called 
  var surfaceCache = {};

  // Public function: adds a cardboard 'card' to the world
  X.card = function (args) {
      //    console.log(args,surfaceCache)
    var src = args.src || "";
    if (surfaceCache[src] || false) {
      var floorMaterial = surfaceCache[src];
    } else {
    	var floorTexture = new THREE.ImageUtils.loadTexture( src );
    	var floorMaterial = new THREE.MeshBasicMaterial( { 
		        side: THREE.DoubleSide /* both sides of this mesh are drawn */ 
      });
      floorMaterial.transparent = true;
      floorMaterial.depthWrite = false // Artifacts also disappear when seting depthTest to false
      floorMaterial.map = floorTexture
      floorMaterial.needsUpdate = true // This is needed
	    surfaceCache[src] = floorMaterial;
    }
    var size   = args.size   || new THREE.Vector2(100,100)
    var anchor = args.anchor || new THREE.Vector2(size.x * 0.5, size.y * 0.5)

    // We always create an anchor, and fasten our surface to this anchor.
    var center = new THREE.Object3D();
    var floorGeometry = new THREE.PlaneGeometry(size.x,size.y);
    var floor = new THREE.Mesh(floorGeometry, floorMaterial);

    console.log("anchor size",anchor,size)

    console.log((0.5 * size.x) - anchor.x ,- (0.5 * size.y) + anchor.y,0);
    floor.position.set((0.5 * size.x) - anchor.x ,-(0.5 * size.y) + anchor.y,0);
    center.add(floor);

    var sphere = new THREE.SphereGeometry( 5, 4, 4 );
    var material = new THREE.MeshBasicMaterial( { color: 0x000000} );
    mesh = new THREE.Mesh( sphere, material );
    center.add(mesh); 
 
    return center;
  }

  // To override with building the scene.
  X.main      = function () {}
  X.animation = function () {}
  
})();

