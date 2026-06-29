struct VertexOutput {
  float4 position: SV_POSITION;
};

struct FragmentOutput {
  float4 color: SV_Target0;  
};

StructuredBuffer<float4> Color : register(t0, space2);

FragmentOutput main(VertexOutput input) {
  FragmentOutput output; 

  if (input.position.y > 100) {
    output.color = Color[0];
  } 
  else {
    output.color = float4(0.0, 1.0, 0.0, 1.0);
  }
  
  return output; 
}
