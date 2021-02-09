(	function () {
	let impacts = [];
	let maxImpactAmount = 32;
	var webglEl = document.getElementById('webgl');
	var width  = window.innerWidth,
		height = window.innerHeight;
	let materialShader;

	// Earth params
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

	let phi = Math.acos(z/radius)
	let theta = Math.atan(y/x);

	console.log(phi)
	console.log(theta)


	let geom = new THREE.SphereBufferGeometry(radius, segments, segments)


	if (!Detector.webgl) {
		Detector.addGetWebGLMessage(webglEl);
		return;
	}

	for (let i = 0; i < maxImpactAmount; i++) {
		impacts.push({
		  impactPosition: new THREE.Vector3().set(
			x,
			y,
			z
		  ),
		  impactMaxRadius: geom.parameters.radius * 4,
		  impactRatio: 0.25
		});
	  }

    var scene = new THREE.Scene();

    scene.fog = new THREE.Fog( 0xcce0ff, 0, 5);
    
    let urls = [
        'assets/textures/Skybox/right.png', 'assets/textures/Skybox/left.png',
        'assets/textures/Skybox/top.png', 'assets/textures/Skybox/bottom.png',
        'assets/textures/Skybox/front.png', 'assets/textures/Skybox/back.png',
      ];
    let loader = new THREE.CubeTextureLoader();
    scene.background = loader.load(urls);

	var camera = new THREE.PerspectiveCamera(45, width / height, 0.01, 1000);
	camera.position.z = 1.5;

	var renderer = new THREE.WebGLRenderer();
	renderer.setSize(width, height);

	scene.add(new THREE.AmbientLight(0x333333));

	const axesHelper = new THREE.AxesHelper( 5 );
	scene.add( axesHelper );

	var light = new THREE.DirectionalLight(0xffffff, 2);
	light.position.set(5,3,5);
	scene.add(light);

	sphereCamera = new THREE.CubeCamera(1,1000,500);
    sphereCamera.position.set(0,1,0);
	scene.add(sphereCamera);

    var sphere = createSphere(geom, sphereCamera);
	sphere.rotation.y = rotation; 
	scene.add(sphere)

    var clouds = createClouds(radius, segments);
	clouds.rotation.y = rotation;
	scene.add(clouds)

	var controls = new THREE.TrackballControls(camera);

	webglEl.appendChild(renderer.domElement);

	runTween();

	render();

	function render() {
		controls.update();
		//sphere.rotation.y += 0.0005;
		//clouds.rotation.y += 0.0005;
		TWEEN.update();		
		renderer.render(scene, camera);
		requestAnimationFrame(render);
	}

	function createSphere(spheregeom, envMap) {
		const textureLoader = new THREE.TextureLoader();
		const material = new THREE.MeshPhongMaterial({
			map:         textureLoader.load('assets/textures/Globe/albedo.jpg'),
			bumpMap:     textureLoader.load('assets/textures/Globe/bump.jpg'),
			bumpScale:   0.02,
			specularMap: textureLoader.load('assets/textures/Globe/specular.png'),
			specular:    new THREE.Color('grey'),
			shininess:   10,
			envMap: envMap.renderTarget.texture								
		});

		material.onBeforeCompile = shader => {
			shader.uniforms.impactPosition = {
			  value: new THREE.Vector3().set(
				x,
				y,
				z
			  )
			};
			shader.uniforms.impactMaxRadius = { value: 0.5 };
			shader.uniforms.impactRatio = { value: 0.25 };
			shader.vertexShader = "varying vec3 vPosition;\n" + shader.vertexShader;
			shader.vertexShader = shader.vertexShader.replace(
			  "#include <worldpos_vertex>",
			  `#include <worldpos_vertex>
			  vPosition = transformed.xyz;`
			);
			shader.fragmentShader =
			  `uniform vec3 impactPosition;\nuniform float impactMaxRadius;\nuniform float impactRatio;\nvarying vec3 vPosition;\n` +
			  shader.fragmentShader;
			shader.fragmentShader = shader.fragmentShader.replace(
			  "#include <dithering_fragment>",
			  `#include <dithering_fragment>
				float dist = distance(vPosition, impactPosition);
				float curRadius = impactMaxRadius * impactRatio;
				float sstep = smoothstep(0., curRadius, dist) - smoothstep(curRadius - ( 0.25 * impactRatio ), curRadius, dist);
				sstep = 1. - sstep * (1. - impactRatio);
				vec3 col = mix(vec3(1., 0.5, 0.0625), vec3(1., 0.25, 0.125), impactRatio);
				gl_FragColor = vec4( mix( col, gl_FragColor.rgb, sstep), diffuseColor.a );`
			);
			materialShader = shader;
		  };
		
		return new THREE.Mesh(spheregeom, material);
	}

	function createClouds(radius, segments) {
        const textureLoader = new THREE.TextureLoader()
		return new THREE.Mesh(
			new THREE.SphereGeometry(radius + 0.003, segments, segments),			
			new THREE.MeshPhongMaterial({
				map:         textureLoader.load('assets/textures/Globe/clouds.png'),
				transparent: true
			})
		);		
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

}());