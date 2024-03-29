var wasm_exports = null;
var wasm_functions = null;
var wasm_memory = null;// new WebAssembly.Memory({initial:1});
var heap = 0;
var gl = null;
var js_objects = [null];
const decoder = new TextDecoder('utf8');
const encoder = new TextEncoder('utf8');

function deref_string(ptr) {
    const data = new BigUint64Array(wasm_memory.buffer, Number(ptr), 2);
    const bytes = new Uint8Array(wasm_memory.buffer, Number(data[0]), Number(data[1]));
    const result = decoder.decode(bytes);
    return result;
}

function deref_function(funcidx) {
    return wasm_functions.get(Number(funcidx));
}

function deref_cstring(ptr) {
    const bytes = new Uint8Array(wasm_memory.buffer, Number(ptr));
    let count = 0;
    for (; count < bytes.length; count++) {
        if (bytes[count] == 0) {
            break;
        }
    }
    const result = decoder.decode(bytes.slice(0, count));
    return result;
}

function deref_u64(ptr) {
    const data = new BigUint64Array(wasm_memory.buffer, Number(offset), 1);
    return data[0];
}

function create_string(data, count) {
    const bytes = new Uint8Array(wasm_memory.buffer, Number(data), Number(count));
    const result = new TextDecoder('utf8').decode(bytes);
    return result;
}

function glAttachShader(program, shader) {
    gl.attachShader(js_objects[program], js_objects[shader]);
}

function glGenBuffers(n, out_buffers) {
    const buffers = new Uint32Array(wasm_memory.buffer, out_buffers, n);
    for (let i = 0; i < n; i++) {
        buffers[i] = js_objects.push(gl.createBuffer()) - 1;
    }
}

function glBindBuffer(target, id) {
    gl.bindBuffer(target, js_objects[id]);
}

function glBufferData(target, size, data, usage) {
    const buf = new Uint8Array(wasm_memory.buffer, data, size);
    gl.bufferData(target, buf, usage);
}

function glEnableVertexAttribArray(attrib) {
    gl.enableVertexAttribArray(attrib);
}

function glGenTextures(n, out_textures) {
    const buffers = new Uint32Array(wasm_memory.buffer, out_textures, n);
    for (let i = 0; i < n; i++) {
        buffers[i] = js_objects.push(gl.createTexture()) - 1;
    }
}

function glBindTexture(target, texture) {
    gl.bindTexture(target, js_objects[texture]);
}

function glActiveTexture(texture) {
    gl.activeTexture(texture);
}

function glTexImage2D(target, level, internalformat, width, height, border, format, type, pixel_ptr) {
    const pixel_size = 4; // TODO: hardcoded pixel size, calculate this from format and type
    const size = width*height*pixel_size;
    const pixels = new Uint8Array(wasm_memory.buffer, Number(pixel_ptr), size)
    gl.texImage2D(target, level, internalformat, width, height, border, format, type, pixels);
}

function glTexParameteri(target, pname, param) {
    gl.texParameteri(target, pname, param);
}

function glDrawArrays(mode, first, count) {
    gl.drawArrays(mode, first, count);
}

function glClear(mask) {
    gl.clear(Number(mask))
}

function glClearColor(r, g, b, a) {
    gl.clearColor(r, g, b, a);
}

function glCompileShader(shader) {
    gl.compileShader(js_objects[shader]);
}

function glCreateBuffer() {
    return BigInt(js_objects.push(gl.createBuffer()) - 1);
}

function glCreateProgram() {
    return BigInt(js_objects.push(gl.createProgram()) - 1);
}

function glCreateShader(shader_type) {
    return BigInt(js_objects.push(gl.createShader(Number(shader_type))) - 1);
}

function glDeleteShader(shader) {
    gl.deleteShader(js_objects[shader]);
}

function glGetAttribLocation(program, pointer, length) {
    const string_data = new Uint8Array(wasm_memory.buffer, pointer, length);
    const string = decoder.decode(string_data);
    return BigInt(gl.getAttribLocation(js_objects[program], string));
}

function glGetShaderiv(shader, type, out_result) {
    const result = new Int32Array(wasm_memory.buffer, out_result, 1);
    result[0] = gl.getShaderParameter(js_objects[shader], type)
}

function glGetProgramiv(program, type, out_result) {
    const result = new Int32Array(wasm_memory.buffer, out_result, 1);
    result[0] = gl.getProgramParameter(js_objects[program], type)
}

function glGetShaderInfoLog(shader, capacity, out_count, out_data) {
    const data = new Uint8Array(wasm_memory.buffer, out_data, capacity);
    const count = new BigUint64Array(wasm_memory.buffer, out_count, 1);
    const string = gl.getShaderInfoLog(js_objects[shader]);
    count[0] = BigInt(string.length);
    encoder.encodeInto(string, data);
}

function glGetProgramInfoLog(program, capacity, out_count, out_data) {
    const data = new Uint8Array(wasm_memory.buffer, out_data, capacity);
    const count = new BigUint64Array(wasm_memory.buffer, out_count, 1);
    const string = gl.getProgramInfoLog(js_objects[program]);
    count[0] = BigInt(string.length);
    encoder.encodeInto(string, data);
}

function glLinkProgram(program) {
    gl.linkProgram(js_objects[program]);
}

function glUseProgram(program) {
    gl.useProgram(js_objects[program]);
}

function glGetUniformLocation(program, cstring) {
    const string = deref_cstring(cstring);
    const uniform = gl.getUniformLocation(js_objects[program], string);
    console.log(string, uniform);
    return BigInt(js_objects.push(uniform) - 1);
}

function glUniform1i(uniform, value) {
    gl.uniform1i(js_objects[uniform], value);
}

function glUniformMatrix4fv(uniform, count, transpose, ptr) {
    const array = new Float32Array(wasm_memory.buffer, ptr, count*16);
    gl.uniformMatrix4fv(js_objects[uniform], transpose, array);
}

function glGetAttribLocation(program, cstring) {
    const string = deref_cstring(cstring);
    return BigInt(gl.getAttribLocation(js_objects[program], string));
}

function glVertexAttribPointer(index, size, type, normalized, stride, offset) {
    gl.vertexAttribPointer(index, size, type, normalized, stride, offset);
}

function glShaderSource(shader, count, string, length) {
    let source = "";

    const dataArray = new BigUint64Array(wasm_memory.buffer, string, count);
    const countArray = new BigUint64Array(wasm_memory.buffer, length, count);
    for (let i = 0; i < count; i++) {
        source += create_string(dataArray[i], countArray[i]);
    }
    console.log(source);
    gl.shaderSource(js_objects[shader], source);
}

function print(ptr) {
    console.log(deref_string(ptr))
}

function webgl_canvas_init() {
    const canvas = document.querySelector("#mainDisplay");
    gl = canvas.getContext("webgl2");

    if (gl === null) {
        alert("Unable to initialize WebGL. Your browser or machine may not support it.");
        return;
    }
}

var main_loop_function = null
const requestAnimationFrame = (function() {
    return window.requestAnimationFrame || 
        window.webkitRequestAnimationFrame ||  
        window.mozRequestAnimationFrame || 
        window.oRequestAnimationFrame || 
        window.msRequestAnimationFrame ||
        
    // if none of the above, use non-native timeout method
    function(callback) {
        window.setTimeout(callback, 1000 / 60);
    };
})();

function main_loop() {
    // console.log(main_loop_function);
    if (main_loop_function) {
        main_loop_function();
        requestAnimationFrame(main_loop);
    }
}

// Auto-generated code for WASM-JS interface
const importObject = {
    gl: {
        glCullFace: (a0) => glCullFace(Number(a0)),
        glFrontFace: (a0) => glFrontFace(Number(a0)),
        glHint: (a0, a1) => glHint(Number(a0), Number(a1)),
        glLineWidth: (a0) => glLineWidth(a0),
        glPointSize: (a0) => glPointSize(a0),
        glPolygonMode: (a0, a1) => glPolygonMode(Number(a0), Number(a1)),
        glScissor: (a0, a1, a2, a3) => glScissor(Number(a0), Number(a1), Number(a2), Number(a3)),
        glTexParameterf: (a0, a1, a2) => glTexParameterf(Number(a0), Number(a1), a2),
        glTexParameterfv: (a0, a1, a2) => glTexParameterfv(Number(a0), Number(a1), Number(a2)),
        glTexParameteri: (a0, a1, a2) => glTexParameteri(Number(a0), Number(a1), Number(a2)),
        glTexParameteriv: (a0, a1, a2) => glTexParameteriv(Number(a0), Number(a1), Number(a2)),
        glTexImage1D: (a0, a1, a2, a3, a4, a5, a6, a7) => glTexImage1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7)),
        glTexImage2D: (a0, a1, a2, a3, a4, a5, a6, a7, a8) => glTexImage2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8)),
        glDrawBuffer: (a0) => glDrawBuffer(Number(a0)),
        glClear: (a0) => glClear(Number(a0)),
        glClearColor: (a0, a1, a2, a3) => glClearColor(a0, a1, a2, a3),
        glClearStencil: (a0) => glClearStencil(Number(a0)),
        glClearDepth: (a0) => glClearDepth(a0),
        glStencilMask: (a0) => glStencilMask(Number(a0)),
        glColorMask: (a0, a1, a2, a3) => glColorMask(Number(a0), Number(a1), Number(a2), Number(a3)),
        glDepthMask: (a0) => glDepthMask(Number(a0)),
        glDisable: (a0) => glDisable(Number(a0)),
        glEnable: (a0) => glEnable(Number(a0)),
        glFinish: () => glFinish(),
        glFlush: () => glFlush(),
        glBlendFunc: (a0, a1) => glBlendFunc(Number(a0), Number(a1)),
        glLogicOp: (a0) => glLogicOp(Number(a0)),
        glStencilFunc: (a0, a1, a2) => glStencilFunc(Number(a0), Number(a1), Number(a2)),
        glStencilOp: (a0, a1, a2) => glStencilOp(Number(a0), Number(a1), Number(a2)),
        glDepthFunc: (a0) => glDepthFunc(Number(a0)),
        glPixelStoref: (a0, a1) => glPixelStoref(Number(a0), a1),
        glPixelStorei: (a0, a1) => glPixelStorei(Number(a0), Number(a1)),
        glReadBuffer: (a0) => glReadBuffer(Number(a0)),
        glReadPixels: (a0, a1, a2, a3, a4, a5, a6) => glReadPixels(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glGetBooleanv: (a0, a1) => glGetBooleanv(Number(a0), Number(a1)),
        glGetDoublev: (a0, a1) => glGetDoublev(Number(a0), Number(a1)),
        glGetError: () => glGetError(),
        glGetFloatv: (a0, a1) => glGetFloatv(Number(a0), Number(a1)),
        glGetIntegerv: (a0, a1) => glGetIntegerv(Number(a0), Number(a1)),
        glGetString: (a0) => glGetString(Number(a0)),
        glGetTexImage: (a0, a1, a2, a3, a4) => glGetTexImage(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetTexParameterfv: (a0, a1, a2) => glGetTexParameterfv(Number(a0), Number(a1), Number(a2)),
        glGetTexParameteriv: (a0, a1, a2) => glGetTexParameteriv(Number(a0), Number(a1), Number(a2)),
        glGetTexLevelParameterfv: (a0, a1, a2, a3) => glGetTexLevelParameterfv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetTexLevelParameteriv: (a0, a1, a2, a3) => glGetTexLevelParameteriv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glIsEnabled: (a0) => glIsEnabled(Number(a0)),
        glDepthRange: (a0, a1) => glDepthRange(a0, a1),
        glViewport: (a0, a1, a2, a3) => glViewport(Number(a0), Number(a1), Number(a2), Number(a3)),
        glDrawArrays: (a0, a1, a2) => glDrawArrays(Number(a0), Number(a1), Number(a2)),
        glDrawElements: (a0, a1, a2, a3) => glDrawElements(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetPointerv: (a0, a1) => glGetPointerv(Number(a0), Number(a1)),
        glPolygonOffset: (a0, a1) => glPolygonOffset(a0, a1),
        glCopyTexImage1D: (a0, a1, a2, a3, a4, a5, a6) => glCopyTexImage1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glCopyTexImage2D: (a0, a1, a2, a3, a4, a5, a6, a7) => glCopyTexImage2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7)),
        glCopyTexSubImage1D: (a0, a1, a2, a3, a4, a5) => glCopyTexSubImage1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glCopyTexSubImage2D: (a0, a1, a2, a3, a4, a5, a6, a7) => glCopyTexSubImage2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7)),
        glTexSubImage1D: (a0, a1, a2, a3, a4, a5, a6) => glTexSubImage1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glTexSubImage2D: (a0, a1, a2, a3, a4, a5, a6, a7, a8) => glTexSubImage2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8)),
        glBindTexture: (a0, a1) => glBindTexture(Number(a0), Number(a1)),
        glDeleteTextures: (a0, a1) => glDeleteTextures(Number(a0), Number(a1)),
        glGenTextures: (a0, a1) => glGenTextures(Number(a0), Number(a1)),
        glIsTexture: (a0) => glIsTexture(Number(a0)),
        glDrawRangeElements: (a0, a1, a2, a3, a4, a5) => glDrawRangeElements(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glTexImage3D: (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) => glTexImage3D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8), Number(a9)),
        glTexSubImage3D: (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => glTexSubImage3D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8), Number(a9), Number(a10)),
        glCopyTexSubImage3D: (a0, a1, a2, a3, a4, a5, a6, a7, a8) => glCopyTexSubImage3D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8)),
        glActiveTexture: (a0) => glActiveTexture(Number(a0)),
        glSampleCoverage: (a0, a1) => glSampleCoverage(a0, Number(a1)),
        glCompressedTexImage3D: (a0, a1, a2, a3, a4, a5, a6, a7, a8) => glCompressedTexImage3D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8)),
        glCompressedTexImage2D: (a0, a1, a2, a3, a4, a5, a6, a7) => glCompressedTexImage2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7)),
        glCompressedTexImage1D: (a0, a1, a2, a3, a4, a5, a6) => glCompressedTexImage1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glCompressedTexSubImage3D: (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => glCompressedTexSubImage3D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8), Number(a9), Number(a10)),
        glCompressedTexSubImage2D: (a0, a1, a2, a3, a4, a5, a6, a7, a8) => glCompressedTexSubImage2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8)),
        glCompressedTexSubImage1D: (a0, a1, a2, a3, a4, a5, a6) => glCompressedTexSubImage1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glGetCompressedTexImage: (a0, a1, a2) => glGetCompressedTexImage(Number(a0), Number(a1), Number(a2)),
        glBlendFuncSeparate: (a0, a1, a2, a3) => glBlendFuncSeparate(Number(a0), Number(a1), Number(a2), Number(a3)),
        glMultiDrawArrays: (a0, a1, a2, a3) => glMultiDrawArrays(Number(a0), Number(a1), Number(a2), Number(a3)),
        glMultiDrawElements: (a0, a1, a2, a3, a4) => glMultiDrawElements(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glPointParameterf: (a0, a1) => glPointParameterf(Number(a0), a1),
        glPointParameterfv: (a0, a1) => glPointParameterfv(Number(a0), Number(a1)),
        glPointParameteri: (a0, a1) => glPointParameteri(Number(a0), Number(a1)),
        glPointParameteriv: (a0, a1) => glPointParameteriv(Number(a0), Number(a1)),
        glBlendColor: (a0, a1, a2, a3) => glBlendColor(a0, a1, a2, a3),
        glBlendEquation: (a0) => glBlendEquation(Number(a0)),
        glGenQueries: (a0, a1) => glGenQueries(Number(a0), Number(a1)),
        glDeleteQueries: (a0, a1) => glDeleteQueries(Number(a0), Number(a1)),
        glIsQuery: (a0) => glIsQuery(Number(a0)),
        glBeginQuery: (a0, a1) => glBeginQuery(Number(a0), Number(a1)),
        glEndQuery: (a0) => glEndQuery(Number(a0)),
        glGetQueryiv: (a0, a1, a2) => glGetQueryiv(Number(a0), Number(a1), Number(a2)),
        glGetQueryObjectiv: (a0, a1, a2) => glGetQueryObjectiv(Number(a0), Number(a1), Number(a2)),
        glGetQueryObjectuiv: (a0, a1, a2) => glGetQueryObjectuiv(Number(a0), Number(a1), Number(a2)),
        glBindBuffer: (a0, a1) => glBindBuffer(Number(a0), Number(a1)),
        glDeleteBuffers: (a0, a1) => glDeleteBuffers(Number(a0), Number(a1)),
        glGenBuffers: (a0, a1) => glGenBuffers(Number(a0), Number(a1)),
        glIsBuffer: (a0) => glIsBuffer(Number(a0)),
        glBufferData: (a0, a1, a2, a3) => glBufferData(Number(a0), Number(a1), Number(a2), Number(a3)),
        glBufferSubData: (a0, a1, a2, a3) => glBufferSubData(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetBufferSubData: (a0, a1, a2, a3) => glGetBufferSubData(Number(a0), Number(a1), Number(a2), Number(a3)),
        glMapBuffer: (a0, a1) => glMapBuffer(Number(a0), Number(a1)),
        glUnmapBuffer: (a0) => glUnmapBuffer(Number(a0)),
        glGetBufferParameteriv: (a0, a1, a2) => glGetBufferParameteriv(Number(a0), Number(a1), Number(a2)),
        glGetBufferPointerv: (a0, a1, a2) => glGetBufferPointerv(Number(a0), Number(a1), Number(a2)),
        glBlendEquationSeparate: (a0, a1) => glBlendEquationSeparate(Number(a0), Number(a1)),
        glDrawBuffers: (a0, a1) => glDrawBuffers(Number(a0), Number(a1)),
        glStencilOpSeparate: (a0, a1, a2, a3) => glStencilOpSeparate(Number(a0), Number(a1), Number(a2), Number(a3)),
        glStencilFuncSeparate: (a0, a1, a2, a3) => glStencilFuncSeparate(Number(a0), Number(a1), Number(a2), Number(a3)),
        glStencilMaskSeparate: (a0, a1) => glStencilMaskSeparate(Number(a0), Number(a1)),
        glAttachShader: (a0, a1) => glAttachShader(Number(a0), Number(a1)),
        glBindAttribLocation: (a0, a1, a2) => glBindAttribLocation(Number(a0), Number(a1), Number(a2)),
        glCompileShader: (a0) => glCompileShader(Number(a0)),
        glCreateProgram: () => glCreateProgram(),
        glCreateShader: (a0) => glCreateShader(Number(a0)),
        glDeleteProgram: (a0) => glDeleteProgram(Number(a0)),
        glDeleteShader: (a0) => glDeleteShader(Number(a0)),
        glDetachShader: (a0, a1) => glDetachShader(Number(a0), Number(a1)),
        glDisableVertexAttribArray: (a0) => glDisableVertexAttribArray(Number(a0)),
        glEnableVertexAttribArray: (a0) => glEnableVertexAttribArray(Number(a0)),
        glGetActiveAttrib: (a0, a1, a2, a3, a4, a5, a6) => glGetActiveAttrib(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glGetActiveUniform: (a0, a1, a2, a3, a4, a5, a6) => glGetActiveUniform(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glGetAttachedShaders: (a0, a1, a2, a3) => glGetAttachedShaders(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetAttribLocation: (a0, a1) => glGetAttribLocation(Number(a0), Number(a1)),
        glGetProgramiv: (a0, a1, a2) => glGetProgramiv(Number(a0), Number(a1), Number(a2)),
        glGetProgramInfoLog: (a0, a1, a2, a3) => glGetProgramInfoLog(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetShaderiv: (a0, a1, a2) => glGetShaderiv(Number(a0), Number(a1), Number(a2)),
        glGetShaderInfoLog: (a0, a1, a2, a3) => glGetShaderInfoLog(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetShaderSource: (a0, a1, a2, a3) => glGetShaderSource(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetUniformLocation: (a0, a1) => glGetUniformLocation(Number(a0), Number(a1)),
        glGetUniformfv: (a0, a1, a2) => glGetUniformfv(Number(a0), Number(a1), Number(a2)),
        glGetUniformiv: (a0, a1, a2) => glGetUniformiv(Number(a0), Number(a1), Number(a2)),
        glGetVertexAttribdv: (a0, a1, a2) => glGetVertexAttribdv(Number(a0), Number(a1), Number(a2)),
        glGetVertexAttribfv: (a0, a1, a2) => glGetVertexAttribfv(Number(a0), Number(a1), Number(a2)),
        glGetVertexAttribiv: (a0, a1, a2) => glGetVertexAttribiv(Number(a0), Number(a1), Number(a2)),
        glGetVertexAttribPointerv: (a0, a1, a2) => glGetVertexAttribPointerv(Number(a0), Number(a1), Number(a2)),
        glIsProgram: (a0) => glIsProgram(Number(a0)),
        glIsShader: (a0) => glIsShader(Number(a0)),
        glLinkProgram: (a0) => glLinkProgram(Number(a0)),
        glShaderSource: (a0, a1, a2, a3) => glShaderSource(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUseProgram: (a0) => glUseProgram(Number(a0)),
        glUniform1f: (a0, a1) => glUniform1f(Number(a0), a1),
        glUniform2f: (a0, a1, a2) => glUniform2f(Number(a0), a1, a2),
        glUniform3f: (a0, a1, a2, a3) => glUniform3f(Number(a0), a1, a2, a3),
        glUniform4f: (a0, a1, a2, a3, a4) => glUniform4f(Number(a0), a1, a2, a3, a4),
        glUniform1i: (a0, a1) => glUniform1i(Number(a0), Number(a1)),
        glUniform2i: (a0, a1, a2) => glUniform2i(Number(a0), Number(a1), Number(a2)),
        glUniform3i: (a0, a1, a2, a3) => glUniform3i(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniform4i: (a0, a1, a2, a3, a4) => glUniform4i(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glUniform1fv: (a0, a1, a2) => glUniform1fv(Number(a0), Number(a1), Number(a2)),
        glUniform2fv: (a0, a1, a2) => glUniform2fv(Number(a0), Number(a1), Number(a2)),
        glUniform3fv: (a0, a1, a2) => glUniform3fv(Number(a0), Number(a1), Number(a2)),
        glUniform4fv: (a0, a1, a2) => glUniform4fv(Number(a0), Number(a1), Number(a2)),
        glUniform1iv: (a0, a1, a2) => glUniform1iv(Number(a0), Number(a1), Number(a2)),
        glUniform2iv: (a0, a1, a2) => glUniform2iv(Number(a0), Number(a1), Number(a2)),
        glUniform3iv: (a0, a1, a2) => glUniform3iv(Number(a0), Number(a1), Number(a2)),
        glUniform4iv: (a0, a1, a2) => glUniform4iv(Number(a0), Number(a1), Number(a2)),
        glUniformMatrix2fv: (a0, a1, a2, a3) => glUniformMatrix2fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix3fv: (a0, a1, a2, a3) => glUniformMatrix3fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix4fv: (a0, a1, a2, a3) => glUniformMatrix4fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glValidateProgram: (a0) => glValidateProgram(Number(a0)),
        glVertexAttrib1d: (a0, a1) => glVertexAttrib1d(Number(a0), a1),
        glVertexAttrib1dv: (a0, a1) => glVertexAttrib1dv(Number(a0), Number(a1)),
        glVertexAttrib1f: (a0, a1) => glVertexAttrib1f(Number(a0), a1),
        glVertexAttrib1fv: (a0, a1) => glVertexAttrib1fv(Number(a0), Number(a1)),
        glVertexAttrib1s: (a0, a1) => glVertexAttrib1s(Number(a0), Number(a1)),
        glVertexAttrib1sv: (a0, a1) => glVertexAttrib1sv(Number(a0), Number(a1)),
        glVertexAttrib2d: (a0, a1, a2) => glVertexAttrib2d(Number(a0), a1, a2),
        glVertexAttrib2dv: (a0, a1) => glVertexAttrib2dv(Number(a0), Number(a1)),
        glVertexAttrib2f: (a0, a1, a2) => glVertexAttrib2f(Number(a0), a1, a2),
        glVertexAttrib2fv: (a0, a1) => glVertexAttrib2fv(Number(a0), Number(a1)),
        glVertexAttrib2s: (a0, a1, a2) => glVertexAttrib2s(Number(a0), Number(a1), Number(a2)),
        glVertexAttrib2sv: (a0, a1) => glVertexAttrib2sv(Number(a0), Number(a1)),
        glVertexAttrib3d: (a0, a1, a2, a3) => glVertexAttrib3d(Number(a0), a1, a2, a3),
        glVertexAttrib3dv: (a0, a1) => glVertexAttrib3dv(Number(a0), Number(a1)),
        glVertexAttrib3f: (a0, a1, a2, a3) => glVertexAttrib3f(Number(a0), a1, a2, a3),
        glVertexAttrib3fv: (a0, a1) => glVertexAttrib3fv(Number(a0), Number(a1)),
        glVertexAttrib3s: (a0, a1, a2, a3) => glVertexAttrib3s(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttrib3sv: (a0, a1) => glVertexAttrib3sv(Number(a0), Number(a1)),
        glVertexAttrib4Nbv: (a0, a1) => glVertexAttrib4Nbv(Number(a0), Number(a1)),
        glVertexAttrib4Niv: (a0, a1) => glVertexAttrib4Niv(Number(a0), Number(a1)),
        glVertexAttrib4Nsv: (a0, a1) => glVertexAttrib4Nsv(Number(a0), Number(a1)),
        glVertexAttrib4Nub: (a0, a1, a2, a3, a4) => glVertexAttrib4Nub(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glVertexAttrib4Nubv: (a0, a1) => glVertexAttrib4Nubv(Number(a0), Number(a1)),
        glVertexAttrib4Nuiv: (a0, a1) => glVertexAttrib4Nuiv(Number(a0), Number(a1)),
        glVertexAttrib4Nusv: (a0, a1) => glVertexAttrib4Nusv(Number(a0), Number(a1)),
        glVertexAttrib4bv: (a0, a1) => glVertexAttrib4bv(Number(a0), Number(a1)),
        glVertexAttrib4d: (a0, a1, a2, a3, a4) => glVertexAttrib4d(Number(a0), a1, a2, a3, a4),
        glVertexAttrib4dv: (a0, a1) => glVertexAttrib4dv(Number(a0), Number(a1)),
        glVertexAttrib4f: (a0, a1, a2, a3, a4) => glVertexAttrib4f(Number(a0), a1, a2, a3, a4),
        glVertexAttrib4fv: (a0, a1) => glVertexAttrib4fv(Number(a0), Number(a1)),
        glVertexAttrib4iv: (a0, a1) => glVertexAttrib4iv(Number(a0), Number(a1)),
        glVertexAttrib4s: (a0, a1, a2, a3, a4) => glVertexAttrib4s(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glVertexAttrib4sv: (a0, a1) => glVertexAttrib4sv(Number(a0), Number(a1)),
        glVertexAttrib4ubv: (a0, a1) => glVertexAttrib4ubv(Number(a0), Number(a1)),
        glVertexAttrib4uiv: (a0, a1) => glVertexAttrib4uiv(Number(a0), Number(a1)),
        glVertexAttrib4usv: (a0, a1) => glVertexAttrib4usv(Number(a0), Number(a1)),
        glVertexAttribPointer: (a0, a1, a2, a3, a4, a5) => glVertexAttribPointer(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glUniformMatrix2x3fv: (a0, a1, a2, a3) => glUniformMatrix2x3fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix3x2fv: (a0, a1, a2, a3) => glUniformMatrix3x2fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix2x4fv: (a0, a1, a2, a3) => glUniformMatrix2x4fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix4x2fv: (a0, a1, a2, a3) => glUniformMatrix4x2fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix3x4fv: (a0, a1, a2, a3) => glUniformMatrix3x4fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniformMatrix4x3fv: (a0, a1, a2, a3) => glUniformMatrix4x3fv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glColorMaski: (a0, a1, a2, a3, a4) => glColorMaski(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetBooleani_v: (a0, a1, a2) => glGetBooleani_v(Number(a0), Number(a1), Number(a2)),
        glGetIntegeri_v: (a0, a1, a2) => glGetIntegeri_v(Number(a0), Number(a1), Number(a2)),
        glEnablei: (a0, a1) => glEnablei(Number(a0), Number(a1)),
        glDisablei: (a0, a1) => glDisablei(Number(a0), Number(a1)),
        glIsEnabledi: (a0, a1) => glIsEnabledi(Number(a0), Number(a1)),
        glBeginTransformFeedback: (a0) => glBeginTransformFeedback(Number(a0)),
        glEndTransformFeedback: () => glEndTransformFeedback(),
        glBindBufferRange: (a0, a1, a2, a3, a4) => glBindBufferRange(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glBindBufferBase: (a0, a1, a2) => glBindBufferBase(Number(a0), Number(a1), Number(a2)),
        glTransformFeedbackVaryings: (a0, a1, a2, a3) => glTransformFeedbackVaryings(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetTransformFeedbackVarying: (a0, a1, a2, a3, a4, a5, a6) => glGetTransformFeedbackVarying(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glClampColor: (a0, a1) => glClampColor(Number(a0), Number(a1)),
        glBeginConditionalRender: (a0, a1) => glBeginConditionalRender(Number(a0), Number(a1)),
        glEndConditionalRender: () => glEndConditionalRender(),
        glVertexAttribIPointer: (a0, a1, a2, a3, a4) => glVertexAttribIPointer(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetVertexAttribIiv: (a0, a1, a2) => glGetVertexAttribIiv(Number(a0), Number(a1), Number(a2)),
        glGetVertexAttribIuiv: (a0, a1, a2) => glGetVertexAttribIuiv(Number(a0), Number(a1), Number(a2)),
        glVertexAttribI1i: (a0, a1) => glVertexAttribI1i(Number(a0), Number(a1)),
        glVertexAttribI2i: (a0, a1, a2) => glVertexAttribI2i(Number(a0), Number(a1), Number(a2)),
        glVertexAttribI3i: (a0, a1, a2, a3) => glVertexAttribI3i(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribI4i: (a0, a1, a2, a3, a4) => glVertexAttribI4i(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glVertexAttribI1ui: (a0, a1) => glVertexAttribI1ui(Number(a0), Number(a1)),
        glVertexAttribI2ui: (a0, a1, a2) => glVertexAttribI2ui(Number(a0), Number(a1), Number(a2)),
        glVertexAttribI3ui: (a0, a1, a2, a3) => glVertexAttribI3ui(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribI4ui: (a0, a1, a2, a3, a4) => glVertexAttribI4ui(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glVertexAttribI1iv: (a0, a1) => glVertexAttribI1iv(Number(a0), Number(a1)),
        glVertexAttribI2iv: (a0, a1) => glVertexAttribI2iv(Number(a0), Number(a1)),
        glVertexAttribI3iv: (a0, a1) => glVertexAttribI3iv(Number(a0), Number(a1)),
        glVertexAttribI4iv: (a0, a1) => glVertexAttribI4iv(Number(a0), Number(a1)),
        glVertexAttribI1uiv: (a0, a1) => glVertexAttribI1uiv(Number(a0), Number(a1)),
        glVertexAttribI2uiv: (a0, a1) => glVertexAttribI2uiv(Number(a0), Number(a1)),
        glVertexAttribI3uiv: (a0, a1) => glVertexAttribI3uiv(Number(a0), Number(a1)),
        glVertexAttribI4uiv: (a0, a1) => glVertexAttribI4uiv(Number(a0), Number(a1)),
        glVertexAttribI4bv: (a0, a1) => glVertexAttribI4bv(Number(a0), Number(a1)),
        glVertexAttribI4sv: (a0, a1) => glVertexAttribI4sv(Number(a0), Number(a1)),
        glVertexAttribI4ubv: (a0, a1) => glVertexAttribI4ubv(Number(a0), Number(a1)),
        glVertexAttribI4usv: (a0, a1) => glVertexAttribI4usv(Number(a0), Number(a1)),
        glGetUniformuiv: (a0, a1, a2) => glGetUniformuiv(Number(a0), Number(a1), Number(a2)),
        glBindFragDataLocation: (a0, a1, a2) => glBindFragDataLocation(Number(a0), Number(a1), Number(a2)),
        glGetFragDataLocation: (a0, a1) => glGetFragDataLocation(Number(a0), Number(a1)),
        glUniform1ui: (a0, a1) => glUniform1ui(Number(a0), Number(a1)),
        glUniform2ui: (a0, a1, a2) => glUniform2ui(Number(a0), Number(a1), Number(a2)),
        glUniform3ui: (a0, a1, a2, a3) => glUniform3ui(Number(a0), Number(a1), Number(a2), Number(a3)),
        glUniform4ui: (a0, a1, a2, a3, a4) => glUniform4ui(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glUniform1uiv: (a0, a1, a2) => glUniform1uiv(Number(a0), Number(a1), Number(a2)),
        glUniform2uiv: (a0, a1, a2) => glUniform2uiv(Number(a0), Number(a1), Number(a2)),
        glUniform3uiv: (a0, a1, a2) => glUniform3uiv(Number(a0), Number(a1), Number(a2)),
        glUniform4uiv: (a0, a1, a2) => glUniform4uiv(Number(a0), Number(a1), Number(a2)),
        glTexParameterIiv: (a0, a1, a2) => glTexParameterIiv(Number(a0), Number(a1), Number(a2)),
        glTexParameterIuiv: (a0, a1, a2) => glTexParameterIuiv(Number(a0), Number(a1), Number(a2)),
        glGetTexParameterIiv: (a0, a1, a2) => glGetTexParameterIiv(Number(a0), Number(a1), Number(a2)),
        glGetTexParameterIuiv: (a0, a1, a2) => glGetTexParameterIuiv(Number(a0), Number(a1), Number(a2)),
        glClearBufferiv: (a0, a1, a2) => glClearBufferiv(Number(a0), Number(a1), Number(a2)),
        glClearBufferuiv: (a0, a1, a2) => glClearBufferuiv(Number(a0), Number(a1), Number(a2)),
        glClearBufferfv: (a0, a1, a2) => glClearBufferfv(Number(a0), Number(a1), Number(a2)),
        glClearBufferfi: (a0, a1, a2, a3) => glClearBufferfi(Number(a0), Number(a1), a2, Number(a3)),
        glGetStringi: (a0, a1) => glGetStringi(Number(a0), Number(a1)),
        glIsRenderbuffer: (a0) => glIsRenderbuffer(Number(a0)),
        glBindRenderbuffer: (a0, a1) => glBindRenderbuffer(Number(a0), Number(a1)),
        glDeleteRenderbuffers: (a0, a1) => glDeleteRenderbuffers(Number(a0), Number(a1)),
        glGenRenderbuffers: (a0, a1) => glGenRenderbuffers(Number(a0), Number(a1)),
        glRenderbufferStorage: (a0, a1, a2, a3) => glRenderbufferStorage(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetRenderbufferParameteriv: (a0, a1, a2) => glGetRenderbufferParameteriv(Number(a0), Number(a1), Number(a2)),
        glIsFramebuffer: (a0) => glIsFramebuffer(Number(a0)),
        glBindFramebuffer: (a0, a1) => glBindFramebuffer(Number(a0), Number(a1)),
        glDeleteFramebuffers: (a0, a1) => glDeleteFramebuffers(Number(a0), Number(a1)),
        glGenFramebuffers: (a0, a1) => glGenFramebuffers(Number(a0), Number(a1)),
        glCheckFramebufferStatus: (a0) => glCheckFramebufferStatus(Number(a0)),
        glFramebufferTexture1D: (a0, a1, a2, a3, a4) => glFramebufferTexture1D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glFramebufferTexture2D: (a0, a1, a2, a3, a4) => glFramebufferTexture2D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glFramebufferTexture3D: (a0, a1, a2, a3, a4, a5) => glFramebufferTexture3D(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glFramebufferRenderbuffer: (a0, a1, a2, a3) => glFramebufferRenderbuffer(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetFramebufferAttachmentParameteriv: (a0, a1, a2, a3) => glGetFramebufferAttachmentParameteriv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGenerateMipmap: (a0) => glGenerateMipmap(Number(a0)),
        glBlitFramebuffer: (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) => glBlitFramebuffer(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6), Number(a7), Number(a8), Number(a9)),
        glRenderbufferStorageMultisample: (a0, a1, a2, a3, a4) => glRenderbufferStorageMultisample(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glFramebufferTextureLayer: (a0, a1, a2, a3, a4) => glFramebufferTextureLayer(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glMapBufferRange: (a0, a1, a2, a3) => glMapBufferRange(Number(a0), Number(a1), Number(a2), Number(a3)),
        glFlushMappedBufferRange: (a0, a1, a2) => glFlushMappedBufferRange(Number(a0), Number(a1), Number(a2)),
        glBindVertexArray: (a0) => glBindVertexArray(Number(a0)),
        glDeleteVertexArrays: (a0, a1) => glDeleteVertexArrays(Number(a0), Number(a1)),
        glGenVertexArrays: (a0, a1) => glGenVertexArrays(Number(a0), Number(a1)),
        glIsVertexArray: (a0) => glIsVertexArray(Number(a0)),
        glDrawArraysInstanced: (a0, a1, a2, a3) => glDrawArraysInstanced(Number(a0), Number(a1), Number(a2), Number(a3)),
        glDrawElementsInstanced: (a0, a1, a2, a3, a4) => glDrawElementsInstanced(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glTexBuffer: (a0, a1, a2) => glTexBuffer(Number(a0), Number(a1), Number(a2)),
        glPrimitiveRestartIndex: (a0) => glPrimitiveRestartIndex(Number(a0)),
        glCopyBufferSubData: (a0, a1, a2, a3, a4) => glCopyBufferSubData(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetUniformIndices: (a0, a1, a2, a3) => glGetUniformIndices(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetActiveUniformsiv: (a0, a1, a2, a3, a4) => glGetActiveUniformsiv(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetActiveUniformName: (a0, a1, a2, a3, a4) => glGetActiveUniformName(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetUniformBlockIndex: (a0, a1) => glGetUniformBlockIndex(Number(a0), Number(a1)),
        glGetActiveUniformBlockiv: (a0, a1, a2, a3) => glGetActiveUniformBlockiv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetActiveUniformBlockName: (a0, a1, a2, a3, a4) => glGetActiveUniformBlockName(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glUniformBlockBinding: (a0, a1, a2) => glUniformBlockBinding(Number(a0), Number(a1), Number(a2)),
        glDrawElementsBaseVertex: (a0, a1, a2, a3, a4) => glDrawElementsBaseVertex(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glDrawRangeElementsBaseVertex: (a0, a1, a2, a3, a4, a5, a6) => glDrawRangeElementsBaseVertex(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glDrawElementsInstancedBaseVertex: (a0, a1, a2, a3, a4, a5) => glDrawElementsInstancedBaseVertex(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glMultiDrawElementsBaseVertex: (a0, a1, a2, a3, a4, a5) => glMultiDrawElementsBaseVertex(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glProvokingVertex: (a0) => glProvokingVertex(Number(a0)),
        glFenceSync: (a0, a1) => glFenceSync(Number(a0), Number(a1)),
        glIsSync: (a0) => glIsSync(Number(a0)),
        glDeleteSync: (a0) => glDeleteSync(Number(a0)),
        glClientWaitSync: (a0, a1, a2) => glClientWaitSync(Number(a0), Number(a1), Number(a2)),
        glWaitSync: (a0, a1, a2) => glWaitSync(Number(a0), Number(a1), Number(a2)),
        glGetInteger64v: (a0, a1) => glGetInteger64v(Number(a0), Number(a1)),
        glGetSynciv: (a0, a1, a2, a3, a4) => glGetSynciv(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4)),
        glGetInteger64i_v: (a0, a1, a2) => glGetInteger64i_v(Number(a0), Number(a1), Number(a2)),
        glGetBufferParameteri64v: (a0, a1, a2) => glGetBufferParameteri64v(Number(a0), Number(a1), Number(a2)),
        glFramebufferTexture: (a0, a1, a2, a3) => glFramebufferTexture(Number(a0), Number(a1), Number(a2), Number(a3)),
        glTexImage2DMultisample: (a0, a1, a2, a3, a4, a5) => glTexImage2DMultisample(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5)),
        glTexImage3DMultisample: (a0, a1, a2, a3, a4, a5, a6) => glTexImage3DMultisample(Number(a0), Number(a1), Number(a2), Number(a3), Number(a4), Number(a5), Number(a6)),
        glGetMultisamplefv: (a0, a1, a2) => glGetMultisamplefv(Number(a0), Number(a1), Number(a2)),
        glSampleMaski: (a0, a1) => glSampleMaski(Number(a0), Number(a1)),
        glBindFragDataLocationIndexed: (a0, a1, a2, a3) => glBindFragDataLocationIndexed(Number(a0), Number(a1), Number(a2), Number(a3)),
        glGetFragDataIndex: (a0, a1) => glGetFragDataIndex(Number(a0), Number(a1)),
        glGenSamplers: (a0, a1) => glGenSamplers(Number(a0), Number(a1)),
        glDeleteSamplers: (a0, a1) => glDeleteSamplers(Number(a0), Number(a1)),
        glIsSampler: (a0) => glIsSampler(Number(a0)),
        glBindSampler: (a0, a1) => glBindSampler(Number(a0), Number(a1)),
        glSamplerParameteri: (a0, a1, a2) => glSamplerParameteri(Number(a0), Number(a1), Number(a2)),
        glSamplerParameteriv: (a0, a1, a2) => glSamplerParameteriv(Number(a0), Number(a1), Number(a2)),
        glSamplerParameterf: (a0, a1, a2) => glSamplerParameterf(Number(a0), Number(a1), a2),
        glSamplerParameterfv: (a0, a1, a2) => glSamplerParameterfv(Number(a0), Number(a1), Number(a2)),
        glSamplerParameterIiv: (a0, a1, a2) => glSamplerParameterIiv(Number(a0), Number(a1), Number(a2)),
        glSamplerParameterIuiv: (a0, a1, a2) => glSamplerParameterIuiv(Number(a0), Number(a1), Number(a2)),
        glGetSamplerParameteriv: (a0, a1, a2) => glGetSamplerParameteriv(Number(a0), Number(a1), Number(a2)),
        glGetSamplerParameterIiv: (a0, a1, a2) => glGetSamplerParameterIiv(Number(a0), Number(a1), Number(a2)),
        glGetSamplerParameterfv: (a0, a1, a2) => glGetSamplerParameterfv(Number(a0), Number(a1), Number(a2)),
        glGetSamplerParameterIuiv: (a0, a1, a2) => glGetSamplerParameterIuiv(Number(a0), Number(a1), Number(a2)),
        glQueryCounter: (a0, a1) => glQueryCounter(Number(a0), Number(a1)),
        glGetQueryObjecti64v: (a0, a1, a2) => glGetQueryObjecti64v(Number(a0), Number(a1), Number(a2)),
        glGetQueryObjectui64v: (a0, a1, a2) => glGetQueryObjectui64v(Number(a0), Number(a1), Number(a2)),
        glVertexAttribDivisor: (a0, a1) => glVertexAttribDivisor(Number(a0), Number(a1)),
        glVertexAttribP1ui: (a0, a1, a2, a3) => glVertexAttribP1ui(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP1uiv: (a0, a1, a2, a3) => glVertexAttribP1uiv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP2ui: (a0, a1, a2, a3) => glVertexAttribP2ui(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP2uiv: (a0, a1, a2, a3) => glVertexAttribP2uiv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP3ui: (a0, a1, a2, a3) => glVertexAttribP3ui(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP3uiv: (a0, a1, a2, a3) => glVertexAttribP3uiv(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP4ui: (a0, a1, a2, a3) => glVertexAttribP4ui(Number(a0), Number(a1), Number(a2), Number(a3)),
        glVertexAttribP4uiv: (a0, a1, a2, a3) => glVertexAttribP4uiv(Number(a0), Number(a1), Number(a2), Number(a3)),
        webgl_canvas_init: () => webgl_canvas_init(),
        webgl_fetch_tex2d: function (target, level, internalformat, src) {
            src = deref_string(src);
            const image = new Image();
            image.addEventListener('load', function () {
                gl.texImage2D(Number(target), Number(level), Number(internalformat), gl.RGBA, gl.UNSIGNED_BYTE, image);
            });
            image.src = src;
        }
    },
    basic: {
        print: (a0) => print(Number(a0)),
    },
    window: {
        start_main_loop: function (callback) {
            main_loop_function = deref_function(callback);
            requestAnimationFrame(main_loop);
        },
        stop_main_loop: function (callback) {
            main_loop_function = null;
        }
    },
    math: {
        cos: function (x) {
            return Math.cos(x);
        },
        sin: function (x) {
            return Math.sin(x);
        },
        tan: function (x) {
            return Math.tan(x);
        }
    }
};

function start() {
    WebAssembly.instantiateStreaming(fetch('simple.wasm'), importObject).then(obj => {
        wasm_exports = obj.instance.exports
        wasm_functions = obj.instance.exports.functions
        wasm_memory = wasm_exports.memory;
        console.log("exited with code " + wasm_exports.main());
    });
}

start();
