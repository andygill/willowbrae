// File:js/Willowbrae.js

/**
 * @author andygill
 */

var WILLOWBRAE = { REVISION: '0' };
var X = { REVISION: '0' };

// Taken from Three.js. Allows the library to be included in various build systems.
if ( typeof define === 'function' && define.amd ) {

		define( 'three', X );

} else if ( 'undefined' !== typeof exports && 'undefined' !== typeof module ) {

		module.exports = X;

}

// Wrapper for the THREE.js library that provides support for building rooms.
// Template originally taken from Three.js "tutorials by example" by Lee Stemkoski,
// though it has been though many changes now.


// Not sure this should all be done in the constructor

// opts.init = function(Theater t) { initial objects }


WILLOWBRAE.Theater = function ( opts ) {
  var that = this

  var keyboard = this.keyboard = new KeyboardState();
  var clock    = this.clock    = new THREE.Clock();
  var fps      = this.fps     = null;

  // GUI
  var gui = this.gui = new GUI();
      
	// SCENE
	var scene = this.scene = new THREE.Scene();

	var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
	var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 20000;

	var camera = this.camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);

	scene.add(camera);
	camera.position.set(0,150,400);
	camera.lookAt(scene.position);	

	if ( Detector.webgl )
		var renderer = this.renderer = new THREE.WebGLRenderer( {antialias:true, alpha: true } );
	else
		var renderer = this.renderer = new THREE.CanvasRenderer(); 
	renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
	container = document.getElementById( 'ThreeJS' );
	container.appendChild( renderer.domElement );
  // EVENTS
  THREEx.WindowResize(renderer, camera);
//  THREEx.FullScreen.bindKey({ charCode : 'm'.charCodeAt(0) });
	// CONTROLS
	var controls = this.controls = new THREE.OrbitControls( camera, renderer.domElement );
	// STATS

	var stats = this.stats = new Stats();
	stats.domElement.style.position = 'absolute';
	stats.domElement.style.bottom = '0px';
	stats.domElement.style.zIndex = 100;
	container.appendChild( stats.domElement );

  if (typeof opts.init == 'function') {
    opts.init(this)
  }

  this.gui.folder("Camera",function() {
    this.add(that, 'fps', { Slow: 1, Slower: 10, "Full Speed": null } );
    this.add(that.camera, 'fov').listen().onChange(function(value) {
      camera.updateProjectionMatrix()
    })
    this.folder('position',function() {
      this.add(that.camera.position, 'x').listen();
      this.add(that.camera.position, 'y').listen();
      this.add(that.camera.position, 'z').listen();
    })
    return true;
  })    
  
  this.animate()

  return this

};

WILLOWBRAE.Theater.prototype = {

	constructor: WILLOWBRAE.Theater,
  
  animate: function ()
  {
    var that = this;
    var f = function () { that.animate() }
    if (this.fps == null) {
      requestAnimationFrame( f )
    } else {
      setTimeout(function () { requestAnimationFrame( f ) },1000 / this.fps)
    }
  	this.render();		
  	this.update();
  },

  render: function () 
  {
  	this.renderer.render( this.scene, this.camera );
  },

  update: function()
  {
//    console.log("X.update")
    this.keyboard.update()

  	if ( this.keyboard.pressed("Z") ) 
  	{	  
      console.log("z pressed")
  		// do something
  	}
	
  	this.controls.update();
  	this.stats.update();
    for(var i in this.children) {
      if (typeof this.children[i].update == 'function') {
        this.children[i].update()
      }
    }

  },


  add: function(e) {
    this.scene.add(e) 
    this.children.push(e)
  },
  
  children: [],

  surfaceCache: {},

  // Public function: adds a cardboard 'card' to the world
  card: function (args) {
      //    console.log(args,surfaceCache)
    var src = args.src || "";
    var size   = args.size   || new THREE.Vector2(100,100)
    var anchor = args.anchor || new THREE.Vector2(size.x * 0.5, size.y * 0.5)
    if (this.surfaceCache[src] || false) {
      var floorMaterial = this.surfaceCache[src];
    } else {
    	var floorTexture = new THREE.ImageUtils.loadTexture( src );
      // This is needed for non-POT images
      floorTexture.minFilter = THREE.LinearFilter
    	var floorMaterial = new THREE.MeshBasicMaterial( { 
		        side: THREE.DoubleSide /* both sides of this mesh are drawn */ 
      });
      floorMaterial.transparent = true;
      floorMaterial.depthWrite = false // Artifacts also disappear when seting depthTest to false
      floorMaterial.map = floorTexture
      floorMaterial.needsUpdate = true // This is needed
	    this.surfaceCache[src] = floorMaterial;
    }

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
 
    center.card = floor
    return center;
  }
  


};


(function () {

  // library-global
  var keyboard = X.keyboard = new KeyboardState();
  var clock = X.clock = new THREE.Clock();
  X.fps = null;

  X.init = function () {


    var gui = X.gui = new GUI();
        
  	// SCENE
  	var scene = X.scene = new THREE.Scene();

  	// CAMERA
  	var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
  	var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 20000;

  	camera = X.camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);
  	scene.add(camera);
  	camera.position.set(0,150,400);
  	camera.lookAt(scene.position);	

  	if ( Detector.webgl )
  		renderer = X.renderer = new THREE.WebGLRenderer( {antialias:true, alpha: true } );
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
      this.add(X, 'fps', { Slow: 1, Slower: 10, "Full Speed": null } );
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

  X.animate = function ()
  {
    if (X.fps == null) {
      requestAnimationFrame( X.animate );
    } else {
      setTimeout(function () { requestAnimationFrame( X.animate ) },1000 / X.fps)
    }
  	X.render();		
  	X.update();
  }


  X.render = function () 
  {
  	X.renderer.render( X.scene, X.camera );
  }

  X.update = function()
  {
//    console.log("X.update")
    X.keyboard.update()

  	if ( X.keyboard.pressed("Z") ) 
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
    var size   = args.size   || new THREE.Vector2(100,100)
    var anchor = args.anchor || new THREE.Vector2(size.x * 0.5, size.y * 0.5)
    if (surfaceCache[src] || false) {
      var floorMaterial = surfaceCache[src];
    } else {
    	var floorTexture = new THREE.ImageUtils.loadTexture( src );
      // This is needed for non-POT images
      floorTexture.minFilter = THREE.LinearFilter
    	var floorMaterial = new THREE.MeshBasicMaterial( { 
		        side: THREE.DoubleSide /* both sides of this mesh are drawn */ 
      });
      floorMaterial.transparent = true;
      floorMaterial.depthWrite = false // Artifacts also disappear when seting depthTest to false
      floorMaterial.map = floorTexture
      floorMaterial.needsUpdate = true // This is needed
	    surfaceCache[src] = floorMaterial;
    }

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
 
    center.card = floor
    return center;
  }

  // To override with building the scene.
  X.main      = function () {}
  X.animation = function () {}
  
})();
