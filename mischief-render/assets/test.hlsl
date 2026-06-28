struct VertexInput {
  float4 position: POSITION;
};

struct VertexOutput {
  float4 position: POSITION;
};

void vertex(in VertexInput IN, out VertexOutput OUT) {
  OUT.position = IN.position;
}

struct FragmentOutput {
  float4 color: SV_Target0;  
};

void fragment(in VertexOutput IN, out FragmentOutput OUT) {
  OUT.color = float4(1.0, 1.0, 1.0, 1.0);
}
