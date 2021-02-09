let impacts = [];
let maxImpactAmount = 32;
var webglEl = document.getElementById('webgl');
var width  = window.innerWidth,
	height = window.innerHeight;
let materialShader;

var radius   = 0.5,
	segments = 64,
	rotation = 0;  

let lat = 9.533330;
let lon = 39.716670;
let latRad = lat * (Math.PI / 180);
let lonRad = -lon * (Math.PI / 180);
let x = radius * Math.cos(latRad) * Math.cos(lonRad);
let y = radius * Math.sin(latRad);
let z = radius * Math.cos(latRad) * Math.sin(lonRad);

let scene;
let camera;
let controls;
let renderer;
let sphereCamera;

//Geometry that we might want to control
let clouds;

function init(){

    if (!Detector.webgl) {
		Detector.addGetWebGLMessage(webglEl);
		return;
	}

    scene = new THREE.Scene();
    scene.fog = new THREE.Fog( 0xcce0ff, 0, 5);
    renderer = new THREE.WebGLRenderer();
    renderer.setSize(width, height);
    scene.background = loadSkybox();


    setCameras()
    addLights();
    createSphereGeometry();

	//const axesHelper = new THREE.AxesHelper( 5 );
	//scene.add( axesHelper );

    webglEl.appendChild(renderer.domElement);

    runTween();

    render();
}

function setCameras()
{
    camera = new THREE.PerspectiveCamera(45, width / height, 0.01, 1000);
    camera.position.z = 1.5;
    controls = new THREE.TrackballControls(camera);
    sphereCamera = new THREE.CubeCamera(1,1000,500);
    sphereCamera.position.set(0,1,0);
	scene.add(sphereCamera);
}

function createSphereGeometry()
{
	scene.add(loadEarth(radius, sphereCamera, new THREE.Vector3().set(x,y,z)));
    clouds = createClouds(radius, 64)
    scene.add(clouds);
}


function addLights()
{
    scene.add(new THREE.AmbientLight(0x333333));
    var light = new THREE.DirectionalLight(0xffffff, 2);
	light.position.set(5,3,5);
	scene.add(light);
}

function render()
{
    controls.update();
    clouds.rotation.y += 0.0001;
	TWEEN.update();		
	renderer.render(scene, camera);
	requestAnimationFrame(render);
}

function runTween() {
    var tween = new TWEEN.Tween({value: 0})
      .to({ value: 1 }, 5000)
      //.easing(TWEEN.Easing.Quintic.Out)
      .onUpdate(val => {
        if (materialShader) materialShader.uniforms.impactRatio.value = val.value;
      })
      .onComplete((val) => {
        if (materialShader) materialShader.uniforms.impactPosition.value.set(
          x,
          y,
         z
        );
        runTween();
      });
    tween.start();
  }