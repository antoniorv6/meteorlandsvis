function loadEarth(radius, envMap, impactVector)
{
    let geom = new THREE.SphereBufferGeometry(radius, 64, 64);
    const material = get_earth_material(impactVector, envMap)
    return new THREE.Mesh(geom, material);
}

function get_earth_material(impactVector, envMap)
{
  const material = new THREE.MeshPhongMaterial({
      map:         loadTexture('assets/textures/Globe/albedo.jpg'),
      bumpMap:     loadTexture('assets/textures/Globe/bump.jpg'),
      bumpScale:   0.02,
      specularMap: loadTexture('assets/textures/Globe/specular.png'),
      specular:    new THREE.Color('grey'),
      shininess:   10,
      envMap: envMap.renderTarget.texture								
  });
  material.onBeforeCompile = shader => {
      shader.uniforms.impactPosition = {
        value: impactVector
      };
      shader.uniforms.impactMaxRadius = { value: impactValue };
      shader.uniforms.impactRatio = { value: 0.25 };
      shader.vertexShader = "varying vec3 vPosition;\n" + shader.vertexShader;
      shader.vertexShader = shader.vertexShader.replace(
        "#include <worldpos_vertex>",
        `#include <worldpos_vertex>
        vPosition = transformed.xyz;`
      );
      shader.fragmentShader =
        `uniform vec3 impactPosition;\n
        uniform float impactMaxRadius;\n
        uniform float impactRatio;\n
        varying vec3 vPosition;\n` +
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

  return material;
}

function createClouds(radius, segments) 
{
  return new THREE.Mesh(
      new THREE.SphereGeometry(radius + 0.003, segments, segments),			
      new THREE.MeshPhongMaterial({
          map:        loadTexture('assets/textures/Globe/clouds.png'),
          transparent: true
      })
  );		
}