Instr("cs.fm.BasicFM", {
  arg freq = 440,
    amp = 0.9,
    gate = 0,
    carrierFreqRatio = 1.0,
    modulatorFreqRatio = 1.0,
    // not sure if having these defaults here actually does anything.
    modEnvShape = Env.adsr(
      attackTime: 0.05,
      decayTime: 0.1,
      sustainLevel: 0.5 * amp,
      releaseTime: 0.1,
      peakLevel: amp,
      curve: [4, -4, -2]
    ),
    carrierEnvShape = Env.adsr(
      attackTime: 0.05,
      decayTime: 0.1,
      sustainLevel: 0.5 * amp,
      releaseTime: 0.1,
      peakLevel: amp,
      curve: [4, -4, -2]
    );

  var carrier,
    modulator,
    carrierEnv,
    modEnv,
    out;

  modEnv = EnvGen.kr(
    envelope: modEnvShape,
    gate: gate
  );
  
  modulator = modEnv * SinOsc.ar(freq * modulatorFreqRatio);
  
  // carrier sustains until noteoff
  carrierEnvShape.releaseNode = 2;

  carrierEnv = EnvGen.kr(
    envelope: carrierEnvShape,
    gate: gate
  );

  carrier = carrierEnv * SinOsc.ar(
    (freq * carrierFreqRatio) + (modulator * freq)
  );

  // free synth when both carrier and modulator envelopes are done
  FreeSelf.kr(Done.kr(carrierEnv) + Done.kr(modEnv) - 1);

  out = amp * carrier;
}, [
  \freq.asSpec(),
  \amp.asSpec(),
  \nil,
  ControlSpec(0.1, 10),
  ControlSpec(0.1, 10),
  EnvSpec(Env.adsr(
    attackTime: 0.05,
    decayTime: 0.1,
    sustainLevel: 0.8,
    releaseTime: 0.1,
    peakLevel: 1.0,
    curve: [4, -4, -2]
  )),
  EnvSpec(Env.adsr(
    attackTime: 0.05,
    decayTime: 0.1,
    sustainLevel: 0.8,
    releaseTime: 0.1,
    peakLevel: 1.0,
    curve: [4, -4, -2]
  ))
]);
