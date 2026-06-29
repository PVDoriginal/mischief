struct VertexInput {
  float4 position: POSITION;
};

struct VertexOutput {
  float4 position: SV_POSITION;
};


void main(in VertexInput IN, out VertexOutput OUT) {
  OUT.position = IN.position;
}

