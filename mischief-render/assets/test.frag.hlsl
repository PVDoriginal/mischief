struct VertexOutput {
  float4 position: SV_POSITION;
};

struct FragmentOutput {
  float4 color: SV_Target0;  
};

void main(in VertexOutput IN, out FragmentOutput OUT) {
  if (IN.position.y > 100) {
    OUT.color = float4(1.0, 0.0, 0.0, 1.0);
  } 
  else {
    OUT.color = float4(0.0, 1.0, 0.0, 1.0);
  }
}
