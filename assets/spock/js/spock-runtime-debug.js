/* config.js - runtime-configuration for SPOCK */


var SPOCK = {
    STACKSIZE: 100, 
    THREADSLICE: 10,
    TRACELENGTH: 32
};
/* runtime.js - SPOCK runtime (javascript part) */


SPOCK.modules = {};
SPOCK.symbolTable = {};
SPOCK.stack = 0;
SPOCK.limit = SPOCK.STACKSIZE;
SPOCK.debug = false;
SPOCK.running = false;
SPOCK.runHook = [];
SPOCK.inBrowser = "document" in this;
SPOCK.global = this;

SPOCK.Continuation = function(func, args) {
    this.k_callee = func;
    this.k_arguments = args;
};

SPOCK.Result = function(val) {
    this.value = val;
};

SPOCK.Symbol = function(name) {
    this.name = name;
    this.plist = {};
};

SPOCK.Pair = function(car, cdr) {
    this.car = car;
    this.cdr = cdr;
};

SPOCK.String = function(chars) {
    if(typeof chars === "string") {
	this.parts = [chars];
	this.length = chars.length;
    }
    else if(typeof chars === "number") this.parts = [chars.toString()];
    else this.parts = chars;	// assumes chars is array
};

SPOCK.Char = function(str) {
    this.character = str.charAt(0);
};

SPOCK.Port = function(direction, methods) {
    var port = this;
    var read = methods.read || function() {
	SPOCK.error("reading from non-input port", port);
    };

    function doread(n) {
	if(n === 0) return "";
	else if(this.peeked) {
	    var p = this.peeked;
	    this.peeked = false;

	    if(n === 1) return p;
	    else return p + read(n - 1);
	}
	else return read(n);
    }

    this.peeked = false;
    this.direction = direction;
    this.read = doread;
    this.write = methods.write || function() { 
	SPOCK.error("writing to non-output port", port) 
    };
    this.close = methods.close || function() {};
    this.flush = methods.flush || function() {};
    this.ready = methods.ready || function() { return true; };
    this.closed = false;
};

SPOCK.Promise = function(thunk) {
    this.thunk = thunk;
};

SPOCK.EndOfFile = function() {};
SPOCK.EOF = new SPOCK.EndOfFile();

SPOCK.check = function(val, type, loc) {
    if(typeof type === "function" && val instanceof type) return val;
    if(typeof val === type) return val;
    else SPOCK.error((loc ? "(" + loc + ") " : "") +
		     "bad argument type" +
		     (typeof type === "string" ? " - expected `" + type + "'" : ""),
		     val);
};

SPOCK.intern = function(str) {
    var old = SPOCK.symbolTable[ str ];

    if(old) return old;
    else return SPOCK.symbolTable[ str ] = new SPOCK.Symbol(str);
};

SPOCK.stringify = function(x, readable) {
    if(readable === undefined) readable = true;

    if(typeof x === "function") return "#<procedure>";
    else if(x === undefined) return "#<undefined>";
    else if(x === null) return "()";
    else if(x instanceof SPOCK.Continuation) return "#<continuation>";
    else if(x instanceof SPOCK.Symbol) return x.name;
    else if(x instanceof SPOCK.Pair) {
	var str = "(";
	var f = false;

	for(var p = x; p !== null && p instanceof SPOCK.Pair; p = p.cdr) {
	    if(f) str += " ";
	
	    str += SPOCK.stringify(p.car, readable);
	    f = true;
	}

	if(p !== null) str += " . " + SPOCK.stringify(p, readable);

	return str + ")";
    }
    else if(x instanceof Array) {
	var str = "#(";
	var f = false;

	for(var i in x) {
	    if(f) str += " ";
	
	    str += SPOCK.stringify(x[ i ], readable);
	    f = true;
	}

	return str + ")";
    }
    else if(x instanceof SPOCK.String) {
	if(readable)
	    return "\"" + x.normalize() + "\""; // XXX does not escape embedded characters
	else return x.normalize();
    }
    else if(x instanceof SPOCK.Char) {
	if(readable) return x.character;

	switch(x.character) {
	case "\n": return "#\\newline";
	case "\t": return "#\\tab";
	case "\r": return "#\\return";
	case " ": return "#\\space";
	default: return "#\\" + x.character;
	}
    }
    else if(x instanceof SPOCK.Port)
	return "#<" + x.direction + " port" + 
	    (x.name ? (" \"" + x.name + "\">") : ">");
    else if(x instanceof SPOCK.Promise) return "#<promise>";
    else if(x instanceof SPOCK.EndOfFile) return "#<eof>";
    else return x.toString();
};

SPOCK.error = function(msg) {
    var args = Array.prototype.splice.call(arguments, 1);

    function argstr(x) {
	return SPOCK.stringify(x, true);
    }

    if(args.length > 0)
	msg = msg + ":\n  " + SPOCK.map(argstr, args).join("\n  ");

    throw new Error(msg);
};

if(this.quit) SPOCK.exit = quit;
else SPOCK.exit = function(code) { 
	SPOCK.error("no suitable primitive available for `exit'");
    };

SPOCK.String.prototype.normalize = function() {
    if(this.parts.length === 0) return "";

    this.parts = [this.parts.join("")];
    return this.parts[ 0 ];
};

SPOCK.jstring = function(x) {
    if(typeof x === "string") return x;
    else if(x instanceof SPOCK.String) return x.normalize();
    else return x;
};

SPOCK.list = function() {
    var lst = null;
    var len = arguments.length;

    for(var i = len - 1; i >= 0; --i)
	lst = new SPOCK.Pair(arguments[ i ], lst);

    return lst;
};

SPOCK.length = function(lst) {
    for(var n = 0; lst instanceof SPOCK.Pair; ++n)
	lst = lst.cdr;

    return n;
};

SPOCK.map = function(func, array) {
    var len = array.length;
    var a2 = new Array(len);

    for(var i in array)
	a2[ i ] = func(array[ i ]);

    return a2;
};

SPOCK.eqvp = function(x, y) {
    if(x === y) return true;
    else if(x instanceof SPOCK.Char) 
	return y instanceof SPOCK.Char && x.character === y.character;
    else return false;
};

SPOCK.equalp = function(x, y) {
    if(x === y) return true;
    else if(x instanceof SPOCK.Pair)
	return y instanceof SPOCK.Pair &&
	    SPOCK.equalp(x.car, y.car) &&
	    SPOCK.equalp(x.cdr, y.cdr);
    else if(x instanceof Array) {
	var len = x.length;
	if(!(y instanceof Array) || y.length != len) return false;
	for(var i = 0; i < len; ++i) {
	    if(!SPOCK.equalp(x[ i ], y[ i ])) return false;
	}
	return true;
    }
    else if(x instanceof SPOCK.Char) 
	return y instanceof SPOCK.Char && x.characters === y.characters;
    else if(x instanceof SPOCK.String) {
	var s1 = x.normalize();

	if(y instanceof SPOCK.String) return s1 === y.normalize();
	else if(typeof y === 'string') return s1 === y;
	else return false;
    }
    else if(typeof x === 'string') {
	if(y instanceof SPOCK.String) return x === y.normalize();
	else if(typeof y === 'string') return x === y;
	else return false;
    }
    else return false;
};

SPOCK.count = function(args, loc) {
    if(--SPOCK.stack <= 0) 
	return new SPOCK.Continuation(args.callee, Array.prototype.slice.call(args));
    else return false;
};

SPOCK.rest = function(args, count, loc) {
    var rest = null;
    
    // this will not unwind, but decrease the counter
    SPOCK.count(args, loc);

    for(var i = args.length - 1; i >= count; --i)
	rest = new SPOCK.Pair(args[ i ], rest);

    return rest;
};

SPOCK.statistics = function() {};

SPOCK.run = function(func) {	// optional arguments
    function terminate(result) {
	return new SPOCK.Result(result);
    }

    var k = terminate;
    var args = [k].concat(Array.prototype.slice.call(arguments, 1));
    var oldstack = SPOCK.stack;
    var oldlimit = SPOCK.limit;
    var oldrunning = SPOCK.running;
    SPOCK.limit = Math.max(10, oldlimit - oldstack);
    SPOCK.stack = SPOCK.limit;
    SPOCK.running = true;

    function restore() {
	SPOCK.stack = oldstack;
	SPOCK.limit = oldlimit;
	SPOCK.running = oldrunning;

	if(!oldrunning) {
	    for(var i in SPOCK.runHook)
		(SPOCK.runHook[ i ])(false);
	}
    }

    var result;

    if(!oldrunning) {
	for(var i in SPOCK.runHook)
	    (SPOCK.runHook[ i ])(true);
    }

    while(true) {
	result = func.apply(SPOCK.global, args);

	if(result instanceof SPOCK.Continuation) {
	    SPOCK.stack = SPOCK.STACKSIZE;
	    func = result.k_callee;
	    args = result.k_arguments;
	}
	else if(result instanceof SPOCK.Result) {
	    restore();
	    return result.value;
	}
	else {
	    restore();
	    SPOCK.error("unexpected return of non-continuation", result);
	}
    }

    return result;
};

SPOCK.callback = function(proc) {
    return function() {
	var args = Array.prototype.slice.call(arguments);
	args.unshift(proc);
	return SPOCK.run.apply(this, args);
    };
};

SPOCK.callbackMethod = function(proc) {
    var g = this;
    return function() {
	var args = Array.prototype.slice.call(arguments);
	args.unshift(this);
	args.unshift(proc);
	return SPOCK.run.apply(g, args);
    };
};

SPOCK.go = function(proc) {
    (SPOCK.callback(proc))();
};

if("java" in this) {				    // rhino
    SPOCK.makeJavaInputPort = function(jp) {
	return new SPOCK.Port("input", {
		read: function(n) {
		    var buffer = ""; 

		    while(n--) {
			var b = jp.read();
			
			if(b === -1) break;
			else buffer += String.fromCharCode(b);
		    }

		    return buffer === "" ? SPOCK.EOF : buffer;
		},

		close: function() { jp.close(); }
	    });
    };
    
    SPOCK.makeJavaOutputPort = function(jp) {
	return new SPOCK.Port("output", {
		write: function(s) {
		    var len = s.length;

		    for(var i = 0; i < len; ++i)
			jp.write(s.charCodeAt(i));
		},

		flush: function() { jp.flush(); },
		close: function() { jp.close(); }
	    });
    };

    SPOCK.log = function() {
	java.lang.System.err.println(Array.prototype.slice.call(arguments).join(""));
    };

    SPOCK.stdin = SPOCK.makeJavaInputPort(java.lang.System[ "in" ]);
    SPOCK.stdout = SPOCK.makeJavaOutputPort(java.lang.System.out);
    SPOCK.stderr = SPOCK.makeJavaOutputPort(java.lang.System.err);
    SPOCK.stderr.name = "[stderr]";
}
else {
    if("console" in this) SPOCK.log = console.log; // firebug
    else if(SPOCK.inBrowser)       // inside browser
	SPOCK.log = function() {
	    var msg = arguments.join(" ");
	
	    if(msg.charAt(msg.length - 1) == "\n")
		msg = msg.substring(0, msg.length - 1);

	    this.defaultStatus = msg;
	};
    else if("print" in this) SPOCK.log = print; // spidermonkey/v8
    else if(typeof process !== undefined) SPOCK.log = console.log; // Node.JS
    else SPOCK.error("no suitable output primitive available");

    (function() {
	var buffer = [];

	function flush() {
	    if(buffer.length > 0) {
		SPOCK.log(buffer.join(""));
		buffer = [];
	    }
	}

	function write(s) {
	    var parts = SPOCK.stringify(s, false).split("\n");
	    var len = parts.length - 1;

	    if(len > 0) {		// contains newline?
		buffer.push(parts[ 0 ]);
		flush();

		if(len > 1) {
		    for(var i = 1; i < len; ++i)
			SPOCK.log(parts[ i ]);
		}

		buffer.push(parts[ len ]);
	    }
	    else buffer.push(parts[ 0 ]);
	}

	SPOCK.stdout = new SPOCK.Port("output", { write: write, flush: flush });
	var inp;
	var ibuffer = "";

	if(this.prompt) {
	    inp = function(n) {
		while(true) {
		    if(ibuffer.length <= n) {
			var part = ibuffer.slice(0, n);
			ibuffer = ibuffer.slice(n);
			return part;
		    }

		    var input = prompt("Expecting input for " + this.toString());
	    
		    if(input === null) return SPOCK.EOF;
		    else ibuffer += input;
		}
	    };
	}
	else {
	    inp = function(n) {
		SPOCK.error("no input possible for standard input port");
	    };
	}

	SPOCK.stdin = new SPOCK.Port("input", { read: inp });
	SPOCK.stderr = SPOCK.stdout;
    })();
}

SPOCK.stdin.name = "[stdin]";
SPOCK.stdout.name = "[stdout]";

SPOCK.flush = function() {
    // note that this always prints a newline when console.log or print is used
    SPOCK.stdout.flush();
    
    if(SPOCK.stderr !== SPOCK.stdout)
	SPOCK.stderr.flush();

    SPOCK.statistics();
};

if(this.gc) SPOCK.gc = gc;
else SPOCK.gc = function() {};

SPOCK.openInputUrlHook = function(url) {
    SPOCK.error("can not open", url);
};

SPOCK.openOutputUrlHook = function(url) {
    SPOCK.error("can not open", url);
};

if("java" in this) {
    SPOCK.openInputFile = function(filename) {
	var stream;

	try {
	    stream = new java.io.FileInputStream(filename);
	}
	catch(e) {
	    SPOCK.error(e.message);
	}

	var port = SPOCK.makeJavaInputPort(stream);
	port.name = filename;
	return port;
    };

    SPOCK.openOutputFile = function(filename) {
	var stream;

	try {
	    stream = new java.io.FileOutputStream(filename);
	}
	catch(e) {
	    SPOCK.error(e.message);
	}

	var port = SPOCK.makeJavaOutputPort(stream);
	port.name = filename;
	return port;
    };

    SPOCK.fileExists = function(filename) {
	return (new java.io.File(filename)).exists();
    };
}
else {
    if(SPOCK.inBrowser) {
	SPOCK.openInputFile = function(filename) {
	    if(filename.match(/^[a-z0-9]+:/)) 
		return SPOCK.openInputUrlHook(filename);
	    
	    var cookies = document.cookie.split("; ");
	    var buffer = null;
	    
	    for(var i = 0; i < cookies.length; ++i) {
		var c = cookies[ i ];
		var p = c.indexOf("=");
		
		if(filename === c.substring(0, p)) {
		    buffer = c.substring(p + 1);
		    break;
		}
	    }

	    if(!buffer) SPOCK.error("can not open file", filename);
	    
	    var pos = 0;
	    
	    return new SPOCK.Port("input", {
		    read: function(n) {
			if(pos >= buffer.length) return SPOCK.EOF;
			else if(pos + len >= buffer.length) 
			    return buffer.substring(pos);
			
			var p1 = pos;
			pos += n;
			return buffer.substring(p1, p1 + n);
		    },
			
			ready: function() { return pos < buffer.length; }
	    });
	};

	SPOCK.openOutputFile = function(filename, expiry) {
	    if(filename.match(/^[a-z0-9]+:/)) 
		return SPOCK.openOutputUrlHook(filename);

	    return new SPOCK.Port("output", {
		    write: function(s) { buffer += s; },
			close: function() {
			var now = (new Date()).getTime();
			var exp = now + (expiry || (1000 * 60 * 60 * 24 * 365));
			document.cookie = filename + "=" + encodeURIComponent(buffer) +
			    "; expires=" + (new Date(exp)).toGMTString();
		    }
	    });
	};
    }
    else {
	SPOCK.openInputFile = function(filename) {
	    SPOCK.error("file-I/O not available");
	}

	SPOCK.openOutputFile = function(filename) {
	    SPOCK.error("file-I/O not available");
	}
    }

    SPOCK.fileExists = function(filename) {
	SPOCK.error("`file-exists?' not available");
    };
}

if("document" in this) {	// browser?
    SPOCK.load = function(url, k) {
	// http://www.nczonline.net/blog/2009/07/28/the-best-way-to-load-external-javascript/
	var script = document.createElement("script")
	
	script.type = "text/javascript";
	k = k || function() {};

	if (script.readyState){  //IE
	    script.onreadystatechange = function(){
		if (script.readyState == "loaded" || script.readyState == "complete"){
		    script.onreadystatechange = null;
		    k(url);
		}
	    };
	} 
	else {  //Others
	    script.onload = function(){
		k(url);
	    };
	}

	script.src = url;
	document.getElementsByTagName("head")[0].appendChild(script);
    };
}
else if("load" in this) {	// rhino/SM
    SPOCK.load = function(filename, k) {
	load(filename);
	
	if(k) k(filename);
    };
}
/* debug.js - debugging-support for SPOCK runtime */


SPOCK.debug = true;
SPOCK.toString = function() { return "#<SPOCK>"; };
SPOCK.restartCount = 0;
SPOCK.traceBuffer = [];
SPOCK.traceOutput = false;
SPOCK.traceHook = [];
SPOCK.hasAlert = SPOCK.inBrowser;

// Overrides SPOCK.error
SPOCK.error = function(msg) {	// "msg" may be a string or an error object
    var args = Array.prototype.splice.call(arguments, 1);
    var err;
    var text;

    if(typeof msg !== "string") { // an object?
	err = msg;
	msg = err.message;
    }

    text = msg;

    if(args.length > 1) 
	text += ":\n  " + SPOCK.map(SPOCK.stringify, args).join("\n  ");

    if(SPOCK.traceBuffer.length > 0) 
	text += "\n\nCall trace:\n\n" + SPOCK.getTrace();

    if(SPOCK.hasAlert) {
	alert("Error: " + text);
	SPOCK.hasAlert = false;	// disable to avoid endless repetition of alerts
    }
    else if(err) throw new (err.constructor)(text);
    else throw new Error(text);
};

// Overrides SPOCK.count
SPOCK.count = function(args, loc) {
    if(--SPOCK.stack <= 0) {
	++SPOCK.restartCount;
	return new SPOCK.Continuation(args.callee, Array.prototype.slice.call(args));
    }

    if(loc) SPOCK.trace(loc, args);

    return false;
};

SPOCK.trace = function(name, args) {
    var tb = SPOCK.traceBuffer;

    for(var i in SPOCK.traceHook) 
	(SPOCK.traceHook[ i ])(name, args);

    if(SPOCK.traceOutput)
	SPOCK.log("[" + SPOCK.stack + "] " + name);

    if(tb.length >= SPOCK.TRACELENGTH) tb.shift();

    tb.push([name, Array.prototype.slice.call(args, 1)]); // skip continuation argument
};

SPOCK.getTrace = function() {
    var tb = SPOCK.traceBuffer;
    var trace = [];

    for(var i in tb) {
	var e = tb[ i ];
	trace.push("  (" + e[ 0 ] + " " +  
		   SPOCK.map(SPOCK.stringify, e[ 1 ]).join(" ") + ")");
    }

    SPOCK.traceBuffer = [];
    return trace.join("\n") + "    <---";
};

// Overrides empty SPOCK.statistics
SPOCK.statistics = function() {
    //if(SPOCK.restartCount > 0)
    //   SPOCK.log("restarts:         ", SPOCK.restartCount);

    SPOCK.traceBuffer = [];
};

// Overrides SPOCK.callback
(function() {
    var old = SPOCK.callback;

    SPOCK.callback = function(proc) {
	var cb = old(proc);
	return function() {
	    var args = Array.prototype.slice.call(arguments);
	    SPOCK.trace("<callback>", args);
	    return cb.apply(this, args);
	};
    };
})();
/* CODE GENERATED BY SPOCK 0 */
var t1336 = function (k1067) {
 var t1337 = function (k1068, t1) {	// %list
  var t1 = SPOCK.rest(arguments, 1, '%list');
  loop: while(true) {
   return k1068(t1);
  }
 };
 ____25list = t1337;	// set! %list
 var t1339 = function(K) {
  SPOCK.count(arguments, 'values');
  return K.apply(SPOCK.global, Array.prototype.slice.call(arguments, 1));
 };
 ___values = t1339;	// set! values
 var t1340 = function(K) {
  SPOCK.count(arguments, 'call-with-values');
  var thunk = arguments[ 1 ];
  var proc = arguments[ 2 ];
  function k2() {
   var args = Array.prototype.slice.call(arguments);
   args.unshift(K);
   return proc.apply(SPOCK.global, args);}
  return thunk(k2);
 };
 ___call_2dwith_2dvalues = t1340;	// set! call-with-values
 var t1341 = function(K) {
  SPOCK.count(arguments, '%call-with-saved-values');
  var t1 = arguments[ 1 ];
  var t2 = arguments[ 2 ];
  var args;
  function k2() { return K.apply(SPOCK.global, args); }
  function k1() {
   args = Array.prototype.slice.call(arguments);
   return t2(k2);}
  return t1(k1);
 };
 ____25call_2dwith_2dsaved_2dvalues = t1341;	// set! %call-with-saved-values
 var t1342 = function (k1069, t2) {	// list?
  var r = SPOCK.count(arguments, "list?");
  if(r) return r;
  var t3 = undefined;
  var t1343 = function (k1070, t4, t5) {	// t3
   var r = SPOCK.count(arguments, "t3");
   if(r) return r;
   loop: while(true) {
    var t1345 = null;
    var t1344 = (t4) === (t1345);
    var t6 = t1344;
    var t1346;
    if(t6 !== false) {
     return k1070(t6);
     t1346 = undefined;
    }
    else {
     var t1348 = (t4) instanceof SPOCK.Pair ;
     var t945 = t1348;
     var t1349;
     if(t945 !== false) {
      var t1350 = t4.cdr;
      var t11 = t1350;
      var t1352 = null;
      var t1351 = (t11) === (t1352);
      var t13 = t1351;
      var t1353;
      if(t13 !== false) {
       return k1070(t13);
       t1353 = undefined;
      }
      else {
       var t1355 = (t11) instanceof SPOCK.Pair ;
       var t946 = t1355;
       var t1356;
       if(t946 !== false) {
        var t1357 = t11.cdr;
        var t18 = t1357;
        var t1358 = t5.cdr;
        var t19 = t1358;
        var t1359 = (t18) === (t19);
        var t22 = t1359;
        var t1360;
        if(t22 !== false) {
         t1360 = false;
        }
        else {
         t1360 = true;
        }
        var t947 = t1360;
        var t1361;
        if(t947 !== false) {
         var t1362 = t18;
         var t1363 = t19;
         t4 = t1362;
         t5 = t1363;
         continue loop;
         t1361 = undefined;
        }
        else {
         return k1070(false);
         t1361 = undefined;
        }
        t1356 = t1361;
       }
       else {
        return k1070(false);
        t1356 = undefined;
       }
       t1353 = t1356;
      }
      t1349 = t1353;
     }
     else {
      return k1070(false);
      t1349 = undefined;
     }
     t1346 = t1349;
    }
   }
  };
  t3 = t1343;	// set! t3
  return t3(k1069, t2, t2);
 };
 ___list_3f = t1342;	// set! list?
 var t1368 = function (k1071, t25) {	// caaar
  var r = SPOCK.count(arguments, "caaar");
  if(r) return r;
  loop: while(true) {
   var t1369 = SPOCK.check(t25, SPOCK.Pair, "caaar");
   var t29 = t1369;
   var t1370 = t29.car;
   var t27 = t1370;
   var t1371 = SPOCK.check(t27, SPOCK.Pair, "caaar");
   var t30 = t1371;
   var t1372 = t30.car;
   var t26 = t1372;
   var t1373 = SPOCK.check(t26, SPOCK.Pair, "caaar");
   var t31 = t1373;
   var t1374 = t31.car;
   return k1071(t1374);
  }
 };
 ___caaar = t1368;	// set! caaar
 var t1376 = function (k1072, t32) {	// caadr
  var r = SPOCK.count(arguments, "caadr");
  if(r) return r;
  loop: while(true) {
   var t1377 = SPOCK.check(t32, SPOCK.Pair, "caadr");
   var t36 = t1377;
   var t1378 = t36.cdr;
   var t34 = t1378;
   var t1379 = SPOCK.check(t34, SPOCK.Pair, "caadr");
   var t37 = t1379;
   var t1380 = t37.car;
   var t33 = t1380;
   var t1381 = SPOCK.check(t33, SPOCK.Pair, "caadr");
   var t38 = t1381;
   var t1382 = t38.car;
   return k1072(t1382);
  }
 };
 ___caadr = t1376;	// set! caadr
 var t1384 = function (k1073, t39) {	// cadar
  var r = SPOCK.count(arguments, "cadar");
  if(r) return r;
  loop: while(true) {
   var t1385 = SPOCK.check(t39, SPOCK.Pair, "cadar");
   var t43 = t1385;
   var t1386 = t43.car;
   var t41 = t1386;
   var t1387 = SPOCK.check(t41, SPOCK.Pair, "cadar");
   var t44 = t1387;
   var t1388 = t44.cdr;
   var t40 = t1388;
   var t1389 = SPOCK.check(t40, SPOCK.Pair, "cadar");
   var t45 = t1389;
   var t1390 = t45.car;
   return k1073(t1390);
  }
 };
 ___cadar = t1384;	// set! cadar
 var t1392 = function (k1074, t46) {	// caddr
  var r = SPOCK.count(arguments, "caddr");
  if(r) return r;
  loop: while(true) {
   var t1393 = SPOCK.check(t46, SPOCK.Pair, "caddr");
   var t50 = t1393;
   var t1394 = t50.cdr;
   var t48 = t1394;
   var t1395 = SPOCK.check(t48, SPOCK.Pair, "caddr");
   var t51 = t1395;
   var t1396 = t51.cdr;
   var t47 = t1396;
   var t1397 = SPOCK.check(t47, SPOCK.Pair, "caddr");
   var t52 = t1397;
   var t1398 = t52.car;
   return k1074(t1398);
  }
 };
 ___caddr = t1392;	// set! caddr
 var t1400 = function (k1075, t53) {	// cdaar
  var r = SPOCK.count(arguments, "cdaar");
  if(r) return r;
  loop: while(true) {
   var t1401 = SPOCK.check(t53, SPOCK.Pair, "cdaar");
   var t57 = t1401;
   var t1402 = t57.car;
   var t55 = t1402;
   var t1403 = SPOCK.check(t55, SPOCK.Pair, "cdaar");
   var t58 = t1403;
   var t1404 = t58.car;
   var t54 = t1404;
   var t1405 = SPOCK.check(t54, SPOCK.Pair, "cdaar");
   var t59 = t1405;
   var t1406 = t59.cdr;
   return k1075(t1406);
  }
 };
 ___cdaar = t1400;	// set! cdaar
 var t1408 = function (k1076, t60) {	// cdadr
  var r = SPOCK.count(arguments, "cdadr");
  if(r) return r;
  loop: while(true) {
   var t1409 = SPOCK.check(t60, SPOCK.Pair, "cdadr");
   var t64 = t1409;
   var t1410 = t64.cdr;
   var t62 = t1410;
   var t1411 = SPOCK.check(t62, SPOCK.Pair, "cdadr");
   var t65 = t1411;
   var t1412 = t65.car;
   var t61 = t1412;
   var t1413 = SPOCK.check(t61, SPOCK.Pair, "cdadr");
   var t66 = t1413;
   var t1414 = t66.cdr;
   return k1076(t1414);
  }
 };
 ___cdadr = t1408;	// set! cdadr
 var t1416 = function (k1077, t67) {	// cddar
  var r = SPOCK.count(arguments, "cddar");
  if(r) return r;
  loop: while(true) {
   var t1417 = SPOCK.check(t67, SPOCK.Pair, "cddar");
   var t71 = t1417;
   var t1418 = t71.car;
   var t69 = t1418;
   var t1419 = SPOCK.check(t69, SPOCK.Pair, "cddar");
   var t72 = t1419;
   var t1420 = t72.cdr;
   var t68 = t1420;
   var t1421 = SPOCK.check(t68, SPOCK.Pair, "cddar");
   var t73 = t1421;
   var t1422 = t73.cdr;
   return k1077(t1422);
  }
 };
 ___cddar = t1416;	// set! cddar
 var t1424 = function (k1078, t74) {	// cdddr
  var r = SPOCK.count(arguments, "cdddr");
  if(r) return r;
  loop: while(true) {
   var t1425 = SPOCK.check(t74, SPOCK.Pair, "cdddr");
   var t78 = t1425;
   var t1426 = t78.cdr;
   var t76 = t1426;
   var t1427 = SPOCK.check(t76, SPOCK.Pair, "cdddr");
   var t79 = t1427;
   var t1428 = t79.cdr;
   var t75 = t1428;
   var t1429 = SPOCK.check(t75, SPOCK.Pair, "cdddr");
   var t80 = t1429;
   var t1430 = t80.cdr;
   return k1078(t1430);
  }
 };
 ___cdddr = t1424;	// set! cdddr
 var t1432 = function (k1079, t81) {	// caaaar
  var r = SPOCK.count(arguments, "caaaar");
  if(r) return r;
  loop: while(true) {
   var t1433 = SPOCK.check(t81, SPOCK.Pair, "caaaar");
   var t86 = t1433;
   var t1434 = t86.car;
   var t84 = t1434;
   var t1435 = SPOCK.check(t84, SPOCK.Pair, "caaaar");
   var t87 = t1435;
   var t1436 = t87.car;
   var t83 = t1436;
   var t1437 = SPOCK.check(t83, SPOCK.Pair, "caaaar");
   var t88 = t1437;
   var t1438 = t88.car;
   var t82 = t1438;
   var t1439 = SPOCK.check(t82, SPOCK.Pair, "caaaar");
   var t89 = t1439;
   var t1440 = t89.car;
   return k1079(t1440);
  }
 };
 ___caaaar = t1432;	// set! caaaar
 var t1442 = function (k1080, t90) {	// caaadr
  var r = SPOCK.count(arguments, "caaadr");
  if(r) return r;
  loop: while(true) {
   var t1443 = SPOCK.check(t90, SPOCK.Pair, "caaadr");
   var t95 = t1443;
   var t1444 = t95.cdr;
   var t93 = t1444;
   var t1445 = SPOCK.check(t93, SPOCK.Pair, "caaadr");
   var t96 = t1445;
   var t1446 = t96.car;
   var t92 = t1446;
   var t1447 = SPOCK.check(t92, SPOCK.Pair, "caaadr");
   var t97 = t1447;
   var t1448 = t97.car;
   var t91 = t1448;
   var t1449 = SPOCK.check(t91, SPOCK.Pair, "caaadr");
   var t98 = t1449;
   var t1450 = t98.car;
   return k1080(t1450);
  }
 };
 ___caaadr = t1442;	// set! caaadr
 var t1452 = function (k1081, t99) {	// caadar
  var r = SPOCK.count(arguments, "caadar");
  if(r) return r;
  loop: while(true) {
   var t1453 = SPOCK.check(t99, SPOCK.Pair, "caadar");
   var t104 = t1453;
   var t1454 = t104.car;
   var t102 = t1454;
   var t1455 = SPOCK.check(t102, SPOCK.Pair, "caadar");
   var t105 = t1455;
   var t1456 = t105.cdr;
   var t101 = t1456;
   var t1457 = SPOCK.check(t101, SPOCK.Pair, "caadar");
   var t106 = t1457;
   var t1458 = t106.car;
   var t100 = t1458;
   var t1459 = SPOCK.check(t100, SPOCK.Pair, "caadar");
   var t107 = t1459;
   var t1460 = t107.car;
   return k1081(t1460);
  }
 };
 ___caadar = t1452;	// set! caadar
 var t1462 = function (k1082, t108) {	// caaddr
  var r = SPOCK.count(arguments, "caaddr");
  if(r) return r;
  loop: while(true) {
   var t1463 = SPOCK.check(t108, SPOCK.Pair, "caaddr");
   var t113 = t1463;
   var t1464 = t113.cdr;
   var t111 = t1464;
   var t1465 = SPOCK.check(t111, SPOCK.Pair, "caaddr");
   var t114 = t1465;
   var t1466 = t114.cdr;
   var t110 = t1466;
   var t1467 = SPOCK.check(t110, SPOCK.Pair, "caaddr");
   var t115 = t1467;
   var t1468 = t115.car;
   var t109 = t1468;
   var t1469 = SPOCK.check(t109, SPOCK.Pair, "caaddr");
   var t116 = t1469;
   var t1470 = t116.car;
   return k1082(t1470);
  }
 };
 ___caaddr = t1462;	// set! caaddr
 var t1472 = function (k1083, t117) {	// cadaar
  var r = SPOCK.count(arguments, "cadaar");
  if(r) return r;
  loop: while(true) {
   var t1473 = SPOCK.check(t117, SPOCK.Pair, "cadaar");
   var t122 = t1473;
   var t1474 = t122.car;
   var t120 = t1474;
   var t1475 = SPOCK.check(t120, SPOCK.Pair, "cadaar");
   var t123 = t1475;
   var t1476 = t123.car;
   var t119 = t1476;
   var t1477 = SPOCK.check(t119, SPOCK.Pair, "cadaar");
   var t124 = t1477;
   var t1478 = t124.cdr;
   var t118 = t1478;
   var t1479 = SPOCK.check(t118, SPOCK.Pair, "cadaar");
   var t125 = t1479;
   var t1480 = t125.car;
   return k1083(t1480);
  }
 };
 ___cadaar = t1472;	// set! cadaar
 var t1482 = function (k1084, t126) {	// cadadr
  var r = SPOCK.count(arguments, "cadadr");
  if(r) return r;
  loop: while(true) {
   var t1483 = SPOCK.check(t126, SPOCK.Pair, "cadadr");
   var t131 = t1483;
   var t1484 = t131.cdr;
   var t129 = t1484;
   var t1485 = SPOCK.check(t129, SPOCK.Pair, "cadadr");
   var t132 = t1485;
   var t1486 = t132.car;
   var t128 = t1486;
   var t1487 = SPOCK.check(t128, SPOCK.Pair, "cadadr");
   var t133 = t1487;
   var t1488 = t133.cdr;
   var t127 = t1488;
   var t1489 = SPOCK.check(t127, SPOCK.Pair, "cadadr");
   var t134 = t1489;
   var t1490 = t134.car;
   return k1084(t1490);
  }
 };
 ___cadadr = t1482;	// set! cadadr
 var t1492 = function (k1085, t135) {	// caddar
  var r = SPOCK.count(arguments, "caddar");
  if(r) return r;
  loop: while(true) {
   var t1493 = SPOCK.check(t135, SPOCK.Pair, "caddar");
   var t140 = t1493;
   var t1494 = t140.car;
   var t138 = t1494;
   var t1495 = SPOCK.check(t138, SPOCK.Pair, "caddar");
   var t141 = t1495;
   var t1496 = t141.cdr;
   var t137 = t1496;
   var t1497 = SPOCK.check(t137, SPOCK.Pair, "caddar");
   var t142 = t1497;
   var t1498 = t142.cdr;
   var t136 = t1498;
   var t1499 = SPOCK.check(t136, SPOCK.Pair, "caddar");
   var t143 = t1499;
   var t1500 = t143.car;
   return k1085(t1500);
  }
 };
 ___caddar = t1492;	// set! caddar
 var t1502 = function (k1086, t144) {	// cadddr
  var r = SPOCK.count(arguments, "cadddr");
  if(r) return r;
  loop: while(true) {
   var t1503 = SPOCK.check(t144, SPOCK.Pair, "cadddr");
   var t149 = t1503;
   var t1504 = t149.cdr;
   var t147 = t1504;
   var t1505 = SPOCK.check(t147, SPOCK.Pair, "cadddr");
   var t150 = t1505;
   var t1506 = t150.cdr;
   var t146 = t1506;
   var t1507 = SPOCK.check(t146, SPOCK.Pair, "cadddr");
   var t151 = t1507;
   var t1508 = t151.cdr;
   var t145 = t1508;
   var t1509 = SPOCK.check(t145, SPOCK.Pair, "cadddr");
   var t152 = t1509;
   var t1510 = t152.car;
   return k1086(t1510);
  }
 };
 ___cadddr = t1502;	// set! cadddr
 var t1512 = function (k1087, t153) {	// cdaaar
  var r = SPOCK.count(arguments, "cdaaar");
  if(r) return r;
  loop: while(true) {
   var t1513 = SPOCK.check(t153, SPOCK.Pair, "cdaaar");
   var t158 = t1513;
   var t1514 = t158.car;
   var t156 = t1514;
   var t1515 = SPOCK.check(t156, SPOCK.Pair, "cdaaar");
   var t159 = t1515;
   var t1516 = t159.car;
   var t155 = t1516;
   var t1517 = SPOCK.check(t155, SPOCK.Pair, "cdaaar");
   var t160 = t1517;
   var t1518 = t160.car;
   var t154 = t1518;
   var t1519 = SPOCK.check(t154, SPOCK.Pair, "cdaaar");
   var t161 = t1519;
   var t1520 = t161.cdr;
   return k1087(t1520);
  }
 };
 ___cdaaar = t1512;	// set! cdaaar
 var t1522 = function (k1088, t162) {	// cdaadr
  var r = SPOCK.count(arguments, "cdaadr");
  if(r) return r;
  loop: while(true) {
   var t1523 = SPOCK.check(t162, SPOCK.Pair, "cdaadr");
   var t167 = t1523;
   var t1524 = t167.cdr;
   var t165 = t1524;
   var t1525 = SPOCK.check(t165, SPOCK.Pair, "cdaadr");
   var t168 = t1525;
   var t1526 = t168.car;
   var t164 = t1526;
   var t1527 = SPOCK.check(t164, SPOCK.Pair, "cdaadr");
   var t169 = t1527;
   var t1528 = t169.car;
   var t163 = t1528;
   var t1529 = SPOCK.check(t163, SPOCK.Pair, "cdaadr");
   var t170 = t1529;
   var t1530 = t170.cdr;
   return k1088(t1530);
  }
 };
 ___cdaadr = t1522;	// set! cdaadr
 var t1532 = function (k1089, t171) {	// cdadar
  var r = SPOCK.count(arguments, "cdadar");
  if(r) return r;
  loop: while(true) {
   var t1533 = SPOCK.check(t171, SPOCK.Pair, "cdadar");
   var t176 = t1533;
   var t1534 = t176.car;
   var t174 = t1534;
   var t1535 = SPOCK.check(t174, SPOCK.Pair, "cdadar");
   var t177 = t1535;
   var t1536 = t177.cdr;
   var t173 = t1536;
   var t1537 = SPOCK.check(t173, SPOCK.Pair, "cdadar");
   var t178 = t1537;
   var t1538 = t178.car;
   var t172 = t1538;
   var t1539 = SPOCK.check(t172, SPOCK.Pair, "cdadar");
   var t179 = t1539;
   var t1540 = t179.cdr;
   return k1089(t1540);
  }
 };
 ___cdadar = t1532;	// set! cdadar
 var t1542 = function (k1090, t180) {	// cdaddr
  var r = SPOCK.count(arguments, "cdaddr");
  if(r) return r;
  loop: while(true) {
   var t1543 = SPOCK.check(t180, SPOCK.Pair, "cdaddr");
   var t185 = t1543;
   var t1544 = t185.cdr;
   var t183 = t1544;
   var t1545 = SPOCK.check(t183, SPOCK.Pair, "cdaddr");
   var t186 = t1545;
   var t1546 = t186.cdr;
   var t182 = t1546;
   var t1547 = SPOCK.check(t182, SPOCK.Pair, "cdaddr");
   var t187 = t1547;
   var t1548 = t187.car;
   var t181 = t1548;
   var t1549 = SPOCK.check(t181, SPOCK.Pair, "cdaddr");
   var t188 = t1549;
   var t1550 = t188.cdr;
   return k1090(t1550);
  }
 };
 ___cdaddr = t1542;	// set! cdaddr
 var t1552 = function (k1091, t189) {	// cddaar
  var r = SPOCK.count(arguments, "cddaar");
  if(r) return r;
  loop: while(true) {
   var t1553 = SPOCK.check(t189, SPOCK.Pair, "cddaar");
   var t194 = t1553;
   var t1554 = t194.car;
   var t192 = t1554;
   var t1555 = SPOCK.check(t192, SPOCK.Pair, "cddaar");
   var t195 = t1555;
   var t1556 = t195.car;
   var t191 = t1556;
   var t1557 = SPOCK.check(t191, SPOCK.Pair, "cddaar");
   var t196 = t1557;
   var t1558 = t196.cdr;
   var t190 = t1558;
   var t1559 = SPOCK.check(t190, SPOCK.Pair, "cddaar");
   var t197 = t1559;
   var t1560 = t197.cdr;
   return k1091(t1560);
  }
 };
 ___cddaar = t1552;	// set! cddaar
 var t1562 = function (k1092, t198) {	// cddadr
  var r = SPOCK.count(arguments, "cddadr");
  if(r) return r;
  loop: while(true) {
   var t1563 = SPOCK.check(t198, SPOCK.Pair, "cddadr");
   var t203 = t1563;
   var t1564 = t203.cdr;
   var t201 = t1564;
   var t1565 = SPOCK.check(t201, SPOCK.Pair, "cddadr");
   var t204 = t1565;
   var t1566 = t204.car;
   var t200 = t1566;
   var t1567 = SPOCK.check(t200, SPOCK.Pair, "cddadr");
   var t205 = t1567;
   var t1568 = t205.cdr;
   var t199 = t1568;
   var t1569 = SPOCK.check(t199, SPOCK.Pair, "cddadr");
   var t206 = t1569;
   var t1570 = t206.cdr;
   return k1092(t1570);
  }
 };
 ___cddadr = t1562;	// set! cddadr
 var t1572 = function (k1093, t207) {	// cdddar
  var r = SPOCK.count(arguments, "cdddar");
  if(r) return r;
  loop: while(true) {
   var t1573 = SPOCK.check(t207, SPOCK.Pair, "cdddar");
   var t212 = t1573;
   var t1574 = t212.car;
   var t210 = t1574;
   var t1575 = SPOCK.check(t210, SPOCK.Pair, "cdddar");
   var t213 = t1575;
   var t1576 = t213.cdr;
   var t209 = t1576;
   var t1577 = SPOCK.check(t209, SPOCK.Pair, "cdddar");
   var t214 = t1577;
   var t1578 = t214.cdr;
   var t208 = t1578;
   var t1579 = SPOCK.check(t208, SPOCK.Pair, "cdddar");
   var t215 = t1579;
   var t1580 = t215.cdr;
   return k1093(t1580);
  }
 };
 ___cdddar = t1572;	// set! cdddar
 var t1582 = function (k1094, t216) {	// cddddr
  var r = SPOCK.count(arguments, "cddddr");
  if(r) return r;
  loop: while(true) {
   var t1583 = SPOCK.check(t216, SPOCK.Pair, "cddddr");
   var t221 = t1583;
   var t1584 = t221.cdr;
   var t219 = t1584;
   var t1585 = SPOCK.check(t219, SPOCK.Pair, "cddddr");
   var t222 = t1585;
   var t1586 = t222.cdr;
   var t218 = t1586;
   var t1587 = SPOCK.check(t218, SPOCK.Pair, "cddddr");
   var t223 = t1587;
   var t1588 = t223.cdr;
   var t217 = t1588;
   var t1589 = SPOCK.check(t217, SPOCK.Pair, "cddddr");
   var t224 = t1589;
   var t1590 = t224.cdr;
   return k1094(t1590);
  }
 };
 ___cddddr = t1582;	// set! cddddr
 var t1592 = function (k1095, t225) {	// append
  var t225 = SPOCK.rest(arguments, 1, 'append');
  var t1594 = null;
  var t1593 = (t225) === (t1594);
  var t948 = t1593;
  var t1595;
  if(t948 !== false) {
   var t1596 = null;
   return k1095(t1596);
   t1595 = undefined;
  }
  else {
   var t229 = undefined;
   var t1598 = function (k1096, t230) {	// t229
    var r = SPOCK.count(arguments, "t229");
    if(r) return r;
    var t1599 = t230.cdr;
    var t231 = t1599;
    var t1601 = null;
    var t1600 = (t231) === (t1601);
    var t949 = t1600;
    var t1602;
    if(t949 !== false) {
     var t1603 = t230.car;
     return k1096(t1603);
     t1602 = undefined;
    }
    else {
     var t236 = undefined;
     var t1605 = function (k1097, t237) {	// t236
      var r = SPOCK.count(arguments, "t236");
      if(r) return r;
      var t1606 = (t237) instanceof SPOCK.Pair ;
      var t950 = t1606;
      var t1607;
      if(t950 !== false) {
       var t1608 = t237.car;
       var t239 = t1608;
       var t1609 = function (t1098) {
        var t240 = t1098;
        var t1610 = new SPOCK.Pair(t239, t240);
        return k1097(t1610);
       };
       var t1612 = t237.cdr;
       return t236(t1609, t1612);
       t1607 = undefined;
      }
      else {
       var t243 = t230;
       var t1614 = t243.cdr;
       return t229(k1097, t1614);
       t1607 = undefined;
      }
     };
     t236 = t1605;	// set! t236
     var t1616 = t230.car;
     return t236(k1096, t1616);
     t1602 = undefined;
    }
   };
   t229 = t1598;	// set! t229
   return t229(k1095, t225);
   t1595 = undefined;
  }
 };
 ___append = t1592;	// set! append
 var t1619 = function (k1099, t245) {	// reverse
  var r = SPOCK.count(arguments, "reverse");
  if(r) return r;
  var t246 = undefined;
  var t1620 = function (k1100, t247, t248) {	// t246
   var r = SPOCK.count(arguments, "t246");
   if(r) return r;
   loop: while(true) {
    var t1621 = (t247) instanceof SPOCK.Pair ;
    var t951 = t1621;
    var t1622;
    if(t951 !== false) {
     var t1625 = t247.cdr;
     var t1623 = t1625;
     var t1626 = t247.car;
     var t251 = t1626;
     var t1627 = new SPOCK.Pair(t251, t248);
     var t1624 = t1627;
     t247 = t1623;
     t248 = t1624;
     continue loop;
     t1622 = undefined;
    }
    else {
     return k1100(t248);
     t1622 = undefined;
    }
   }
  };
  t246 = t1620;	// set! t246
  var t1629 = null;
  return t246(k1099, t245, t1629);
 };
 ___reverse = t1619;	// set! reverse
 var t1631 = function (k1101, t254, t255) {	// list-tail
  var r = SPOCK.count(arguments, "list-tail");
  if(r) return r;
  var t256 = undefined;
  var t1632 = function (k1102, t257, t258) {	// t256
   var r = SPOCK.count(arguments, "t256");
   if(r) return r;
   loop: while(true) {
    var t1634 = (t257)  <= 0 ;
    var t1633;
    if(t1634 !== false) {
     return k1102(t258);
     t1633 = undefined;
    }
    else {
     var t1638 = (t257)  - 1 ;
     var t1636 = t1638;
     var t1639 = SPOCK.check(t258, SPOCK.Pair, "t256");
     var t259 = t1639;
     var t1640 = t259.cdr;
     var t1637 = t1640;
     t257 = t1636;
     t258 = t1637;
     continue loop;
     t1633 = undefined;
    }
   }
  };
  t256 = t1632;	// set! t256
  var t1641 = SPOCK.check(t255, 'number', "list-tail");
  return t256(k1101, t1641, t254);
 };
 ___list_2dtail = t1631;	// set! list-tail
 var t260 = ___list_2dtail;
 var t1643 = function (k1103, t261, t262) {	// t260
  var r = SPOCK.count(arguments, "t260");
  if(r) return r;
  var t1644 = function (t1105) {
   var t1645 = SPOCK.check(t1105, SPOCK.Pair);
   var t1104 = t1645;
   var t263 = t1104;
   var t1646 = t263.car;
   return k1103(t1646);
  };
  return t260(t1644, t261, t262);
 };
 ___list_2dref = t1643;	// set! list-ref
 var t1649 = function(K) {
  SPOCK.count(arguments, 'memq');
  var x = arguments[ 1 ];
  for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {
    if(n.car === x) return K(n);
  }
  return K(false);
 };
 ___memq = t1649;	// set! memq
 var t1650 = function(K) {
  SPOCK.count(arguments, 'memv');
  var x = arguments[ 1 ];
  for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {
    if(SPOCK.eqvp(n.car, x)) return K(n);
  }
  return K(false);
 };
 ___memv = t1650;	// set! memv
 var t1651 = function(K) {
  SPOCK.count(arguments, 'member');
  var x = arguments[ 1 ];
  for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {
    if(SPOCK.equalp(n.car, x)) return K(n);
  }
  return K(false);
 };
 ___member = t1651;	// set! member
 var t1652 = function(K) {
  SPOCK.count(arguments, 'assq');
  var x = arguments[ 1 ];
  for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {
    var p = n.car;
    if(p instanceof SPOCK.Pair && p.car === x) return K(p);
  }
  return K(false);
 };
 ___assq = t1652;	// set! assq
 var t1653 = function(K) {
  SPOCK.count(arguments, 'assv');
  var x = arguments[ 1 ];
  for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {
    var p = n.car;
    if(p instanceof SPOCK.Pair && SPOCK.eqvp(p.car, x)) return K(p);
  }
  return K(false);
 };
 ___assv = t1653;	// set! assv
 var t1654 = function(K) {
  SPOCK.count(arguments, 'assoc');
  var x = arguments[ 1 ];
  for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {
    var p = n.car;
    if(p instanceof SPOCK.Pair && SPOCK.equalp(p.car, x)) return K(p);
  }
  return K(false);
 };
 ___assoc = t1654;	// set! assoc
 var t1655 = function(K) {
  SPOCK.count(arguments, '%+');
  var len = arguments.length;
  switch(len) {
  case 1: return K(0);
  case 2: return K(SPOCK.check(arguments[ 1 ], 'number', '+'));
  default:
   var p = SPOCK.check(arguments[ 1 ], 'number', '+');
   for(var i = 2; i < len; ++i) {
    p += SPOCK.check(arguments[ i ], 'number', '+');
   }
   return K(p);}
 };
 ____25_2b = t1655;	// set! %+
 var t1656 = function(K) {
  SPOCK.count(arguments, '%-');
  var len = arguments.length;
  switch(len) {
  case 1: SPOCK.error('(-) bad argument count', len);
  case 2: return K(-SPOCK.check(arguments[ 1 ], 'number', '-'));
  default:
   var p = SPOCK.check(arguments[ 1 ], 'number', '-');
   for(var i = 2; i < len; ++i) {
    p -= SPOCK.check(arguments[ i ], 'number', '-');
   }
   return K(p);}
 };
 ____25_2d = t1656;	// set! %-
 var t1657 = function(K) {
  SPOCK.count(arguments, '%*');
  var len = arguments.length;
  switch(len) {
  case 1: return K(1);
  case 2: return K(SPOCK.check(arguments[ 1 ], 'number', '*'));
  default:
   var p = SPOCK.check(arguments[ 1 ], 'number', '*');
   for(var i = 2; i < len; ++i) {
    p *= SPOCK.check(arguments[ i ], 'number', '*');
   }
   return K(p);}
 };
 ____25_2a = t1657;	// set! %*
 var t1658 = function(K) {
  SPOCK.count(arguments, '%/');
  var len = arguments.length;
  switch(len) {
  case 1: SPOCK.error('(/) bad argument count', len);
  case 2: return K(1/SPOCK.check(arguments[ 1 ], 'number', '/'));
  default:
   var p = SPOCK.check(arguments[ 1 ], 'number', '/');
   for(var i = 2; i < len; ++i) {
    p /= SPOCK.check(arguments[ i ], 'number', '/');
   }
   return K(p);}
 };
 ____25_2f = t1658;	// set! %/
 var t1659 = function(K) {
  SPOCK.count(arguments, '%=');
  var argc = arguments.length;
  var last = SPOCK.check(arguments[ 1 ], 'number', '=');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', '=');
   if(last !== x) return K(false);
   else last = x;}
  return K(true);
 };
 ____25_3d = t1659;	// set! %=
 var t1660 = function(K) {
  SPOCK.count(arguments, '%>');
  var argc = arguments.length;
  var last = SPOCK.check(arguments[ 1 ], 'number', '>');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', '>');
   if(last <= x) return K(false);
   else last = x;}
  return K(true);
 };
 ____25_3e = t1660;	// set! %>
 var t1661 = function(K) {
  SPOCK.count(arguments, '%<');
  var argc = arguments.length;
  var last = SPOCK.check(arguments[ 1 ], 'number', '<');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', '<');
   if(last >= x) return K(false);
   else last = x;}
  return K(true);
 };
 ____25_3c = t1661;	// set! %<
 var t1662 = function(K) {
  SPOCK.count(arguments, '%>=');
  var argc = arguments.length;
  var last = SPOCK.check(arguments[ 1 ], 'number', '>=');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', '>=');
   if(last < x) return K(false);
   else last = x;}
  return K(true);
 };
 ____25_3e_3d = t1662;	// set! %>=
 var t1663 = function(K) {
  SPOCK.count(arguments, '%<=');
  var argc = arguments.length;
  var last = SPOCK.check(arguments[ 1 ], 'number', '<=');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', '<=');
   if(last > x) return K(false);
   else last = x;}
  return K(true);
 };
 ____25_3c_3d = t1663;	// set! %<=
 var t1664 = function(K) {
  SPOCK.count(arguments, '%max');
  var argc = arguments.length;
  var n = SPOCK.check(arguments[ 1 ], 'number', 'max');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', 'max');
   if(n < x) n = x;}
  return K(n);
 };
 ____25max = t1664;	// set! %max
 var t1665 = function(K) {
  SPOCK.count(arguments, '%min');
  var argc = arguments.length;
  var n = SPOCK.check(arguments[ 1 ], 'number', 'max');
  for(var i = 2; i < argc; ++i) {
   var x = SPOCK.check(arguments[ i ], 'number', 'max');
   if(n > x) n = x;}
  return K(n);
 };
 ____25min = t1665;	// set! %min
 var t1666 = function (k1106, t264, t265) {	// modulo
  var r = SPOCK.count(arguments, "modulo");
  if(r) return r;
  loop: while(true) {
   var t1668 = SPOCK.check(t264, 'number', "modulo");
   var t1669 = SPOCK.check(t265, 'number', "modulo");
   var t1667 = (t1668)  /  (t1669);
   var t275 = t1667;
   var t1670 = SPOCK.check(t275, 'number', "modulo");
   var t1672 = (t275)  < 0 ;
   var t1671;
   if(t1672 !== false) {
    var t1673 = Math.ceil(t275);
    t1671 = t1673;
   }
   else {
    var t1674 = Math.floor(t275);
    t1671 = t1674;
   }
   var t271 = t1671;
   var t1676 = SPOCK.check(t271, 'number', "modulo");
   var t1677 = SPOCK.check(t265, 'number', "modulo");
   var t1675 = (t1676)  *  (t1677);
   var t270 = t1675;
   var t1679 = SPOCK.check(t264, 'number', "modulo");
   var t1680 = SPOCK.check(t270, 'number', "modulo");
   var t1678 = (t1679)  -  (t1680);
   var t266 = t1678;
   var t1682 = SPOCK.check(t265, 'number', "modulo");
   var t1683 = SPOCK.check(0, 'number', "modulo");
   var t1681 = (t1682)  <  (t1683);
   var t952 = t1681;
   var t1684;
   if(t952 !== false) {
    var t1686 = SPOCK.check(t266, 'number', "modulo");
    var t1687 = SPOCK.check(0, 'number', "modulo");
    var t1685 = (t1686)  <=  (t1687);
    var t953 = t1685;
    var t1688;
    if(t953 !== false) {
     t1688 = t266;
    }
    else {
     var t1690 = SPOCK.check(t266, 'number', "modulo");
     var t1691 = SPOCK.check(t265, 'number', "modulo");
     var t1689 = (t1690)  +  (t1691);
     t1688 = t1689;
    }
    t1684 = t1688;
   }
   else {
    var t1693 = SPOCK.check(t266, 'number', "modulo");
    var t1694 = SPOCK.check(0, 'number', "modulo");
    var t1692 = (t1693)  >=  (t1694);
    var t954 = t1692;
    var t1695;
    if(t954 !== false) {
     t1695 = t266;
    }
    else {
     var t1697 = SPOCK.check(t266, 'number', "modulo");
     var t1698 = SPOCK.check(t265, 'number', "modulo");
     var t1696 = (t1697)  +  (t1698);
     t1695 = t1696;
    }
    t1684 = t1695;
   }
   return k1106(t1684);
  }
 };
 ___modulo = t1666;	// set! modulo
 var t1700 = function (k1107, t289, t290) {	// t288
  var r = SPOCK.count(arguments, "t288");
  if(r) return r;
  loop: while(true) {
   var t1702 = SPOCK.check(t289, 'number', "t288");
   var t1703 = SPOCK.check(t290, 'number', "t288");
   var t1701 = (t1702)  /  (t1703);
   var t297 = t1701;
   var t1704 = SPOCK.check(t297, 'number', "t288");
   var t1706 = (t297)  < 0 ;
   var t1705;
   if(t1706 !== false) {
    var t1707 = Math.ceil(t297);
    t1705 = t1707;
   }
   else {
    var t1708 = Math.floor(t297);
    t1705 = t1708;
   }
   var t293 = t1705;
   var t1710 = SPOCK.check(t293, 'number', "t288");
   var t1711 = SPOCK.check(t290, 'number', "t288");
   var t1709 = (t1710)  *  (t1711);
   var t292 = t1709;
   var t1713 = SPOCK.check(t289, 'number', "t288");
   var t1714 = SPOCK.check(t292, 'number', "t288");
   var t1712 = (t1713)  -  (t1714);
   return k1107(t1712);
  }
 };
 var t288 = t1700;
 var t1716 = function (k1108, t300, t301) {	// t288
  var r = SPOCK.count(arguments, "t288");
  if(r) return r;
  var t302 = undefined;
  var t1717 = function (k1109, t303, t304) {	// t302
   var r = SPOCK.count(arguments, "t302");
   if(r) return r;
   var t1718 = SPOCK.check(t304, 'number', "t302");
   var t307 = t1718;
   var t1719 = (0) === (t307);
   var t955 = t1719;
   var t1720;
   if(t955 !== false) {
    var t1722 = SPOCK.check(t303, 'number', "t302");
    var t1721 = Math.abs(t1722);
    return k1109(t1721);
    t1720 = undefined;
   }
   else {
    var t1724 = function (t1110) {
     return t302(k1109, t304, t1110);
    };
    return t288(t1724, t303, t304);
    t1720 = undefined;
   }
  };
  t302 = t1717;	// set! t302
  return t302(k1108, t300, t301);
 };
 ____25gcd = t1716;	// set! %gcd
 var t1728 = function (k1111, t309) {	// gcd
  var t309 = SPOCK.rest(arguments, 1, 'gcd');
  var t1730 = null;
  var t1729 = (t309) === (t1730);
  var t956 = t1729;
  var t1731;
  if(t956 !== false) {
   return k1111(0);
   t1731 = undefined;
  }
  else {
   var t313 = undefined;
   var t1733 = function (k1112, t314, t315) {	// t313
    var r = SPOCK.count(arguments, "t313");
    if(r) return r;
    var t1734 = t314.car;
    var t316 = t1734;
    var t1735 = t314.cdr;
    var t317 = t1735;
    var t1736;
    if(t315 !== false) {
     var t1737 = SPOCK.check(t316, 'number', "t313");
     t1736 = t1737;
    }
    else {
     t1736 = undefined;
    }
    var t1739 = null;
    var t1738 = (t317) === (t1739);
    var t957 = t1738;
    var t1740;
    if(t957 !== false) {
     var t1742 = SPOCK.check(t316, 'number', "t313");
     var t1741 = Math.abs(t1742);
     return k1112(t1741);
     t1740 = undefined;
    }
    else {
     var t1744 = t317.car;
     var t324 = t1744;
     var t1745 = SPOCK.check(t324, 'number', "t313");
     var t1746 = function (t1114) {
      var t326 = t1114;
      var t1747 = t317.cdr;
      var t327 = t1747;
      var t1748 = new SPOCK.Pair(t326, t327);
      var t1113 = t1748;
      return t313(k1112, t1113, false);
     };
     return ____25gcd(t1746, t316, t324);
     t1740 = undefined;
    }
   };
   t313 = t1733;	// set! t313
   return t313(k1111, t309, true);
   t1731 = undefined;
  }
 };
 ___gcd = t1728;	// set! gcd
 var t1752 = function (k1115, t329, t330) {	// %lcm
  var r = SPOCK.count(arguments, "%lcm");
  if(r) return r;
  var t1754 = SPOCK.check(t329, 'number', "%lcm");
  var t1755 = SPOCK.check(t330, 'number', "%lcm");
  var t1753 = (t1754)  *  (t1755);
  var t331 = t1753;
  var t1756 = function (t1116) {
   var t332 = t1116;
   var t1758 = SPOCK.check(t331, 'number');
   var t1759 = SPOCK.check(t332, 'number');
   var t1757 = (t1758)  /  (t1759);
   var t335 = t1757;
   var t1760 = SPOCK.check(t335, 'number');
   var t1762 = (t335)  < 0 ;
   var t1761;
   if(t1762 !== false) {
    var t1763 = Math.ceil(t335);
    t1761 = t1763;
   }
   else {
    var t1764 = Math.floor(t335);
    t1761 = t1764;
   }
   return k1115(t1761);
  };
  return ____25gcd(t1756, t329, t330);
 };
 ____25lcm = t1752;	// set! %lcm
 var t1767 = function (k1117, t338) {	// lcm
  var t338 = SPOCK.rest(arguments, 1, 'lcm');
  var t1769 = null;
  var t1768 = (t338) === (t1769);
  var t958 = t1768;
  var t1770;
  if(t958 !== false) {
   return k1117(1);
   t1770 = undefined;
  }
  else {
   var t342 = undefined;
   var t1772 = function (k1118, t343, t344) {	// t342
    var r = SPOCK.count(arguments, "t342");
    if(r) return r;
    var t1773 = t343.car;
    var t345 = t1773;
    var t1774 = t343.cdr;
    var t346 = t1774;
    var t1775;
    if(t344 !== false) {
     var t1776 = SPOCK.check(t345, 'number', "t342");
     t1775 = t1776;
    }
    else {
     t1775 = undefined;
    }
    var t1778 = null;
    var t1777 = (t346) === (t1778);
    var t959 = t1777;
    var t1779;
    if(t959 !== false) {
     var t1781 = SPOCK.check(t345, 'number', "t342");
     var t1780 = Math.abs(t1781);
     return k1118(t1780);
     t1779 = undefined;
    }
    else {
     var t1783 = t346.car;
     var t353 = t1783;
     var t1784 = SPOCK.check(t353, 'number', "t342");
     var t1785 = function (t1120) {
      var t355 = t1120;
      var t1786 = t346.cdr;
      var t356 = t1786;
      var t1787 = new SPOCK.Pair(t355, t356);
      var t1119 = t1787;
      return t342(k1118, t1119, false);
     };
     return ____25lcm(t1785, t345, t353);
     t1779 = undefined;
    }
   };
   t342 = t1772;	// set! t342
   return t342(k1117, t338, true);
   t1770 = undefined;
  }
 };
 ___lcm = t1767;	// set! lcm
 var t1791 = function(K) {
  SPOCK.count(arguments, 'string->symbol');
  var str = SPOCK.jstring(arguments[ 1 ]);
  return K(SPOCK.intern(str));
 };
 ___string_2d_3esymbol = t1791;	// set! string->symbol
 var t1792 = function (k1121, t358, t359) {	// get
  var r = SPOCK.count(arguments, "get");
  if(r) return r;
  loop: while(true) {
   var t1794 = SPOCK.check(t358, SPOCK.Symbol, "get");
   var t1796 = SPOCK.check(t359, SPOCK.Symbol, "get");
   var t1795 = t1796.name;
   var t1793 = (t1794) .plist[ (t1795) ] ;
   var t360 = t1793;
   var t1797 = t360 === undefined;
   var t361 = t1797;
   var t1798;
   if(t361 !== false) {
    t1798 = false;
   }
   else {
    t1798 = true;
   }
   var t960 = t1798;
   var t1799;
   if(t960 !== false) {
    t1799 = t360;
   }
   else {
    t1799 = false;
   }
   return k1121(t1799);
  }
 };
 ___get = t1792;	// set! get
 var t1801 = function (k1122, t363, t364, t365) {	// put!
  var r = SPOCK.count(arguments, "put!");
  if(r) return r;
  loop: while(true) {
   var t1803 = SPOCK.check(t363, SPOCK.Symbol, "put!");
   var t1805 = SPOCK.check(t364, SPOCK.Symbol, "put!");
   var t1804 = t1805.name;
   var t1802 = (t1803) .plist[ (t1804) ] =  (t365);
   return k1122(t1802);
  }
 };
 ___put_21 = t1801;	// set! put!
 var t1807 = function(K) {
  SPOCK.count(arguments, 'string-append');
  var args = Array.prototype.slice.call(arguments, 1);
  var strs = SPOCK.map(function(x) { return SPOCK.jstring(x); }, args);
  return K(new SPOCK.String(strs));
 };
 ___string_2dappend = t1807;	// set! string-append
 var t1808 = function(K) {
  SPOCK.count(arguments, 'string');
  var str = [];
  var len = arguments.length - 1;
  for(var i = 1; i <= len; ++i) {
   var x = arguments[ i ];
   if(x instanceof SPOCK.Char) str.push(x.character);
   else SPOCK.error('bad argument type - not a character', x);}
  return K(new SPOCK.String(str.join('')));
 };
 ___string = t1808;	// set! string
 var t1809 = function(K) {
  SPOCK.count(arguments, 'string->list');
  var str = SPOCK.jstring(arguments[ 1 ]);
  var lst = null;
  var len = str.length;
  for(var i = len - 1; i >= 0; --i)
   lst = new SPOCK.Pair(new SPOCK.Char(str.charAt(i)), lst);
  return K(lst);
 };
 ___string_2d_3elist = t1809;	// set! string->list
 var t1810 = function(K) {
  SPOCK.count(arguments, 'list->string');
  var lst = arguments[ 1 ];
  var str = [];
  while(lst instanceof SPOCK.Pair) {
   str.push(SPOCK.check(lst.car, SPOCK.Char).character);
   lst = lst.cdr;}
  return K(new SPOCK.String(str.join('')));
 };
 ___list_2d_3estring = t1810;	// set! list->string
 var t1811 = function(K) {
  SPOCK.count(arguments, 'make-string');
  var n = SPOCK.check(arguments[ 1 ], 'number', 'make-string');
  var c = arguments[ 2 ];
  var a = new Array(n);
  if(c !== undefined)
   c = SPOCK.check(c, SPOCK.Char, 'make-string').character;
  else c = ' ';
  for(var i = 0; i < n; ++i) a[ i ] = c;
  return K(new SPOCK.String(a.join('')));
 };
 ___make_2dstring = t1811;	// set! make-string
 var t1812 = function(K) {
  SPOCK.count(arguments, 'string-ref');
  var str = arguments[ 1 ];
  var i = SPOCK.check(arguments[ 2 ], 'number', 'string-ref');
  if(typeof str === 'string')
   return K(new SPOCK.Char(str.charAt(i)));
  else if(str instanceof SPOCK.String) {
   var parts = str.parts;
   for(var p in parts) {
    var l = parts[ p ].length;
    if(i <= l) return K(new SPOCK.Char(parts[ p ].charAt(i)));
    else i -= l;}
   SPOCK.error('`string-ref\' out of range', str, i);}
 };
 ___string_2dref = t1812;	// set! string-ref
 var t1813 = function(K) {
  SPOCK.count(arguments, 'string-set!');
  var str = arguments[ 1 ];
  var i = SPOCK.check(arguments[ 2 ], 'number', 'string-set!');
  var c = SPOCK.check(arguments[ 3 ], SPOCK.Char, 'string-set!');
  if(typeof str === 'string')
   SPOCK.error('argument to `string-set!\' is not a mutable string', str);
  else if(str instanceof SPOCK.String) {
   var parts = str.parts;
   for(var p in parts) {
    var part = parts[ p ];
    var l = part.length;
    if(i <= l) {
     parts[ p ] = part.substring(0, i) + c.character + part.substring(i + 1);
     return K(undefined);
    } else i -= l;}
   SPOCK.error('`string-set!\' out of range', str, i);}
 };
 ___string_2dset_21 = t1813;	// set! string-set!
 var t1814 = function (k1123, t366, t367, t368) {	// string-copy
  var r = SPOCK.count(arguments, "string-copy");
  if(r) return r;
  loop: while(true) {
   var t1815 = SPOCK.jstring(t366);
   var t369 = t1815;
   var t1816 = t367 === undefined;
   var t961 = t1816;
   var t1817;
   if(t961 !== false) {
    t1817 = 0;
   }
   else {
    var t1818 = SPOCK.check(t367, 'number', "string-copy");
    t1817 = t1818;
   }
   var t371 = t1817;
   var t1819 = t368 === undefined;
   var t962 = t1819;
   var t1820;
   if(t962 !== false) {
    var t1821 = t369.length;
    t1820 = t1821;
   }
   else {
    var t1822 = SPOCK.check(t368, 'number', "string-copy");
    t1820 = t1822;
   }
   var t373 = t1820;
   var t1823 = t369.slice(t371, t373);
   var t375 = t1823;
   var t1824 = new SPOCK.String(t375);
   return k1123(t1824);
  }
 };
 ___string_2dcopy = t1814;	// set! string-copy
 var t1826 = function (k1124, t376, t377, t378, t379) {	// string-fill!
  var r = SPOCK.count(arguments, "string-fill!");
  if(r) return r;
  var t1827 = (t376) instanceof SPOCK.String ;
  var t380 = t1827;
  var t1828;
  if(t380 !== false) {
   t1828 = false;
  }
  else {
   t1828 = true;
  }
  var t963 = t1828;
  var t1829 = function (t1125) {	// t1126
   var t1830 = t376.normalize();
   var t381 = t1830;
   var t1831 = SPOCK.check(t377, SPOCK.Char, "t1126");
   var t382 = t1831;
   var t1832 = t378 === undefined;
   var t964 = t1832;
   var t1833;
   if(t964 !== false) {
    t1833 = 0;
   }
   else {
    var t1834 = SPOCK.check(t378, 'number', "t1126");
    t1833 = t1834;
   }
   var t383 = t1833;
   var t1835 = t379 === undefined;
   var t965 = t1835;
   var t1836;
   if(t965 !== false) {
    var t1837 = t381.length;
    t1836 = t1837;
   }
   else {
    var t1838 = SPOCK.check(t379, 'number', "t1126");
    t1836 = t1838;
   }
   var t385 = t1836;
   var t1839 = function(K) {
    SPOCK.count(arguments);
    var str = arguments[ 1 ];
    var from = arguments[ 2 ];
    var to = arguments[ 3 ];
    var c = arguments[ 4 ];
    var snew = new Array(to - from);
    for(var i in snew) snew[ i ] = c;
    str.parts = [str.parts[ 0 ].substring(0, from), snew.join(''),
     str.parts[ 0 ].substring(to)];
    return K(str);
   };
   return t1839(k1124, t376, t383, t385, t382);
  };
  var t1126 = t1829;
  var t1841;
  if(t963 !== false) {
   return ____25error(t1126, "bad argument type - not a mutable string", t376);
   t1841 = undefined;
  }
  else {
   return t1126(undefined);
   t1841 = undefined;
  }
 };
 ___string_2dfill_21 = t1826;	// set! string-fill!
 var t1844 = function(K) {
  SPOCK.count(arguments, 'vector');
  return K(Array.prototype.slice.call(arguments, 1));
 };
 ___vector = t1844;	// set! vector
 var t1845 = function(K) {
  SPOCK.count(arguments, 'make-vector');
  var n = SPOCK.check(arguments[ 1 ], 'number', 'make-vector');
  var x = arguments[ 2 ];
  var a = new Array(n);
  if(x !== undefined) {
   for(var i = 0; i < n; ++i) a[ i ] = x;}
  return K(a);
 };
 ___make_2dvector = t1845;	// set! make-vector
 var t1846 = function(K) {
  SPOCK.count(arguments, 'vector->list');
  var vec = SPOCK.check(arguments[ 1 ], Array, 'vector->list');
  var lst = null;
  var len = vec.length;
  for(var i = len - 1; i >= 0; --i)
   lst = new SPOCK.Pair(vec[ i ], lst);
  return K(lst);
 };
 ___vector_2d_3elist = t1846;	// set! vector->list
 var t1847 = function(K) {
  SPOCK.count(arguments, 'list->vector');
  var lst = arguments[ 1 ];
  var vec = [];
  while(lst instanceof SPOCK.Pair) {
   vec.push(lst.car);
   lst = lst.cdr;}
  return K(vec);
 };
 ___list_2d_3evector = t1847;	// set! list->vector
 var t1848 = function(K) {
  SPOCK.count(arguments, 'vector-fill!');
  var vec = SPOCK.check(arguments[ 1 ], Array, 'vector-fill!');
  var x = arguments[ 2 ];
  var from = arguments[ 3 ];
  var to = arguments[ 4 ];
  if(from === undefined) from = 0;
  if(to === undefined) to = vec.length;
  for(var i = from; i < to; ++i)
   vec[ i ] = x;
  return K(undefined);
 };
 ___vector_2dfill_21 = t1848;	// set! vector-fill!
 var t1849 = function(K) {
  SPOCK.count(arguments, 'string->number');
  var str = SPOCK.jstring(arguments[ 1 ]);
  var base = arguments[ 2 ];
  if(!base) base = 10;
  else base = SPOCK.check(base, 'number', 'string->number');
  var m = true, neg = 1;
  while(m) {
   m = str.match(/^#[eboxid]/);
   if(m) {
    switch(str[ 1 ]) {
    case 'e':
    case 'i': break;
    case 'd': base = 10; break;
    case 'o': base = 8; break;
    case 'x': base = 16; break;
    case 'b': base = 2; break;
    default: return K(false);}
    str = str.substring(2);}}
  switch(str[ 0 ]) {
  case '-': neg = -1; str = str.substring(1); break;
  case '+': str = str.substring(1);}
  var num, den = false;
  if((m = str.match(/^([^\/]+)\/(.+)$/))) {
    str = m[ 1 ];
    den = m[ 2 ];}
  function num3(s) {
   var tr = null;
   switch(base) {
   case 2: tr = /^[0-1]+$/; break;
   case 8: tr = /^[0-7]+$/; break;
   case 10: tr = /^[#0-9]*\.?[#0-9]+([esdfl][-+]?[0-9]+)?$/; break;
   case 16: tr = /^[0-9a-fA-F]+$/;}
   if(tr && !s.match(tr)) return false;
   var s2 = s.replace(/#/g, '0');
   if(base === 10) s2 = parseFloat(s2.replace(/[esdfl]/g, 'e'));
   else if(s2 !== s) return false;
   else s2 = parseInt(s2, base);
   return isNaN(s2) ? false : s2;}
  if((num = num3(str)) === false) return K(false);
  if(den && !(den = num3(den))) return K(false);
  return K(neg * num / (den || 1));
 };
 ___string_2d_3enumber = t1849;	// set! string->number
 var t1850 = function(K) {
  SPOCK.count(arguments, '%show');
  arguments[ 2 ].write(arguments[ 1 ]);
  return K(undefined);
 };
 ____25show = t1850;	// set! %show
 var t1851 = function(K) {
  SPOCK.count(arguments, '%fetch');
  return K(arguments[ 2 ].read(arguments[ 1 ]));
 };
 ____25fetch = t1851;	// set! %fetch
 var t1852 = function(K) {
  SPOCK.count(arguments, '%check-port');
  var port = arguments[ 1 ];
  var dir = arguments[ 2 ];
  if(port instanceof SPOCK.Port) {
   if(port.closed)
    SPOCK.error('port is already closed', port);
   else if(port.direction !== dir)
    SPOCK.error('bad argument type - not an ' + dir + ' port', port, arguments[ 3 ]);
  }
  else SPOCK.error('bad argument type - not a port', port, arguments[ 3 ]);
  return K(port);
 };
 ____25check_2dport = t1852;	// set! %check-port
 var t1853 = function (k1127, t387) {	// newline
  var r = SPOCK.count(arguments, "newline");
  if(r) return r;
  var t1854 = t387 === undefined;
  var t966 = t1854;
  var t1855 = function (t1128) {	// t1129
   return ____25show(k1127, "\n", t1128);
  };
  var t1129 = t1855;
  var t1857;
  if(t966 !== false) {
   return t1129(SPOCK.stdout);
   t1857 = undefined;
  }
  else {
   return ____25check_2dport(t1129, t387, "output", "newline");
   t1857 = undefined;
  }
 };
 ___newline = t1853;	// set! newline
 var t1860 = function (k1130, t389) {	// read-char
  var r = SPOCK.count(arguments, "read-char");
  if(r) return r;
  var t1861 = t389 === undefined;
  var t967 = t1861;
  var t1862 = function (t1132) {	// t1133
   var t1863 = function (t1131) {
    var t390 = t1131;
    var t394 = SPOCK.EOF;
    var t1864 = (t390) === (t394);
    var t968 = t1864;
    var t1865;
    if(t968 !== false) {
     t1865 = t390;
    }
    else {
     var t1866 = new SPOCK.Char(t390);
     t1865 = t1866;
    }
    return k1130(t1865);
   };
   return ____25fetch(t1863, 1, t1132);
  };
  var t1133 = t1862;
  var t1869;
  if(t967 !== false) {
   return t1133(SPOCK.stdin);
   t1869 = undefined;
  }
  else {
   return ____25check_2dport(t1133, t389, "input", "read-char");
   t1869 = undefined;
  }
 };
 ___read_2dchar = t1860;	// set! read-char
 var t1872 = function (k1134, t395, t396) {	// write-char
  var r = SPOCK.count(arguments, "write-char");
  if(r) return r;
  var t1873 = t396 === undefined;
  var t969 = t1873;
  var t1874 = function (t1135) {	// t1136
   var t1876 = SPOCK.check(t395, SPOCK.Char, "t1136");
   var t1875 = t1876.character;
   return ____25show(k1134, t1875, t1135);
  };
  var t1136 = t1874;
  var t1878;
  if(t969 !== false) {
   return t1136(SPOCK.stdout);
   t1878 = undefined;
  }
  else {
   return ____25check_2dport(t1136, t396, "output", "write-char");
   t1878 = undefined;
  }
 };
 ___write_2dchar = t1872;	// set! write-char
 var t398 = ___read_2dchar;
 var t1881 = function (k1137, t399) {	// t398
  var r = SPOCK.count(arguments, "t398");
  if(r) return r;
  var t1882 = function (t1138) {
   var t400 = t1138;
   var t404 = SPOCK.EOF;
   var t1883 = (t400) === (t404);
   var t401 = t1883;
   var t1884;
   if(t401 !== false) {
    t1884 = false;
   }
   else {
    t1884 = true;
   }
   var t970 = t1884;
   var t1885;
   if(t970 !== false) {
    var t1887 = t400.character;
    var t1886 = (t399) .peeked =  (t1887);
    t1885 = t1886;
   }
   else {
    t1885 = undefined;
   }
   return k1137(t400);
  };
  return t398(t1882, t399);
 };
 ___peek_2dchar = t1881;	// set! peek-char
 var t1890 = function (k1139, t405) {	// char-ready?
  var r = SPOCK.count(arguments, "char-ready?");
  if(r) return r;
  var t1891 = function (t1140) {
   var t1892 = t405.ready();
   return k1139(t1892);
  };
  return ____25check_2dport(t1891, t405, "input", "char-ready?");
 };
 ___char_2dready_3f = t1890;	// set! char-ready?
 var t1895 = function (k1141, t406, t407, t408) {	// %print-hook
  var r = SPOCK.count(arguments, "%print-hook");
  if(r) return r;
  return ____25show(k1141, "#<unknown object>", t407);
 };
 ____25print_2dhook = t1895;	// set! %print-hook
 var t1897 = function (k1142, t409, t410) {	// display
  var r = SPOCK.count(arguments, "display");
  if(r) return r;
  var t1898 = t410 === undefined;
  var t971 = t1898;
  var t1899 = function (t1143) {	// t1157
   var t411 = t1143;
   var t413 = undefined;
   var t1900 = function (k1144, t414) {	// t413
    var r = SPOCK.count(arguments, "t413");
    if(r) return r;
    var t1902 = null;
    var t1901 = (t414) === (t1902);
    var t972 = t1901;
    var t1903;
    if(t972 !== false) {
     return ____25show(k1144, "()", t411);
     t1903 = undefined;
    }
    else {
     var t1905 = typeof(t414);
     var t419 = t1905;
     var t1906 = (t419) === ("number");
     var t973 = t1906;
     var t1907;
     if(t973 !== false) {
      var t1910 = SPOCK.check(t414, 'number', "t413");
      var t1911 = undefined === undefined;
      var t974 = t1911;
      var t1912;
      if(t974 !== false) {
       t1912 = 10;
      }
      else {
       var t1913 = SPOCK.check(undefined, 'number', "t413");
       t1912 = t1913;
      }
      var t1909 = t1910.toString(t1912);
      var t1908 = new SPOCK.String(t1909);
      var t421 = t1908;
      var t1914 = SPOCK.jstring(t421);
      return ____25show(k1144, t1914, t411);
      t1907 = undefined;
     }
     else {
      var t1916 = typeof(t414);
      var t427 = t1916;
      var t1917 = (t427) === ("string");
      var t426 = t1917;
      var t1918;
      if(t426 !== false) {
       t1918 = t426;
      }
      else {
       var t1919 = (t414) instanceof SPOCK.String ;
       t1918 = t1919;
      }
      var t975 = t1918;
      var t1920;
      if(t975 !== false) {
       var t1921 = SPOCK.jstring(t414);
       return ____25show(k1144, t1921, t411);
       t1920 = undefined;
      }
      else {
       var t1923 = (t414) instanceof SPOCK.Symbol ;
       var t976 = t1923;
       var t1924;
       if(t976 !== false) {
        var t1925 = t414.name;
        return ____25show(k1144, t1925, t411);
        t1924 = undefined;
       }
       else {
        var t1927 = (t414) instanceof SPOCK.Char ;
        var t977 = t1927;
        var t1928;
        if(t977 !== false) {
         var t1929 = t414.character;
         return ____25show(k1144, t1929, t411);
         t1928 = undefined;
        }
        else {
         var t433 = SPOCK.EOF;
         var t1931 = (t414) === (t433);
         var t978 = t1931;
         var t1932;
         if(t978 !== false) {
          return ____25show(k1144, "#<eof>", t411);
          t1932 = undefined;
         }
         else {
          var t1934 = typeof(t414);
          var t435 = t1934;
          var t1935 = (t435) === ("function");
          var t979 = t1935;
          var t1936;
          if(t979 !== false) {
           return ____25show(k1144, "#<procedure>", t411);
           t1936 = undefined;
          }
          else {
           var t1938 = (t414) === (true);
           var t438 = t1938;
           var t1939;
           if(t438 !== false) {
            t1939 = t438;
           }
           else {
            var t1940 = (t414) === (false);
            t1939 = t1940;
           }
           var t980 = t1939;
           var t1941;
           if(t980 !== false) {
            var t1942;
            if(t414 !== false) {
             t1942 = "#t";
            }
            else {
             t1942 = "#f";
            }
            return ____25show(k1144, t1942, t411);
            t1941 = undefined;
           }
           else {
            var t1944 = (t414) instanceof SPOCK.Pair ;
            var t981 = t1944;
            var t1945;
            if(t981 !== false) {
             var t1946 = function (t1145) {
              var t444 = undefined;
              var t1947 = function (k1146, t445) {	// t444
               var r = SPOCK.count(arguments, "t444");
               if(r) return r;
               var t1949 = null;
               var t1948 = (t445) === (t1949);
               var t982 = t1948;
               var t1950;
               if(t982 !== false) {
                return ____25show(k1146, ")", t411);
                t1950 = undefined;
               }
               else {
                var t1952 = (t445) instanceof SPOCK.Pair ;
                var t449 = t1952;
                var t1953;
                if(t449 !== false) {
                 t1953 = false;
                }
                else {
                 t1953 = true;
                }
                var t983 = t1953;
                var t1954;
                if(t983 !== false) {
                 var t1955 = function (t1147) {
                  var t1956 = function (t1148) {
                   return ____25show(k1146, ")", t411);
                  };
                  return t413(t1956, t445);
                 };
                 return ____25show(t1955, " . ", t411);
                 t1954 = undefined;
                }
                else {
                 var t452 = t414;
                 var t1960 = (t452) === (t445);
                 var t451 = t1960;
                 var t1961;
                 if(t451 !== false) {
                  t1961 = false;
                 }
                 else {
                  t1961 = true;
                 }
                 var t984 = t1961;
                 var t1962 = function (t1149) {	// t1151
                  var t1963 = function (t1150) {
                   var t1964 = SPOCK.check(t445, SPOCK.Pair);
                   var t456 = t1964;
                   var t1965 = t456.cdr;
                   return t444(k1146, t1965);
                  };
                  var t1967 = t445.car;
                  return t413(t1963, t1967);
                 };
                 var t1151 = t1962;
                 var t1969;
                 if(t984 !== false) {
                  return ____25show(t1151, " ", t411);
                  t1969 = undefined;
                 }
                 else {
                  return t1151(undefined);
                  t1969 = undefined;
                 }
                 t1954 = t1969;
                }
                t1950 = t1954;
               }
              };
              t444 = t1947;	// set! t444
              return t444(k1144, t414);
             };
             return ____25show(t1946, "(", t411);
             t1945 = undefined;
            }
            else {
             var t1974 = t414 === undefined;
             var t985 = t1974;
             var t1975;
             if(t985 !== false) {
              return ____25show(k1144, "#<undefined>", t411);
              t1975 = undefined;
             }
             else {
              var t1977 = (t414) instanceof Array ;
              var t986 = t1977;
              var t1978;
              if(t986 !== false) {
               var t1979 = t414.length;
               var t459 = t1979;
               var t1980 = function (t1152) {
                var t460 = undefined;
                var t1981 = function (k1153, t461) {	// t460
                 var r = SPOCK.count(arguments, "t460");
                 if(r) return r;
                 var t1983 = (t461) >= (t459);
                 var t1982;
                 if(t1983 !== false) {
                  return ____25show(k1153, ")", t411);
                  t1982 = undefined;
                 }
                 else {
                  var t1985 = (t461) === (0);
                  var t462 = t1985;
                  var t1986;
                  if(t462 !== false) {
                   t1986 = false;
                  }
                  else {
                   t1986 = true;
                  }
                  var t987 = t1986;
                  var t1987 = function (t1154) {	// t1156
                   var t1988 = function (t1155) {
                    var t1989 =  1+ (t461);
                    return t460(k1153, t1989);
                   };
                   var t1991 = (t414) [ (t461) ] ;
                   return t413(t1988, t1991);
                  };
                  var t1156 = t1987;
                  var t1993;
                  if(t987 !== false) {
                   return ____25show(t1156, " ", t411);
                   t1993 = undefined;
                  }
                  else {
                   return t1156(undefined);
                   t1993 = undefined;
                  }
                  t1982 = t1993;
                 }
                };
                t460 = t1981;	// set! t460
                return t460(k1144, 0);
               };
               return ____25show(t1980, "#(", t411);
               t1978 = undefined;
              }
              else {
               var t1999 = (t414) instanceof SPOCK.Port ;
               var t1998;
               if(t1999 !== false) {
                var t2000 = SPOCK.stringify(t414);
                return ____25show(k1144, t2000, t411);
                t1998 = undefined;
               }
               else {
                var t2003 = (t414) instanceof SPOCK.Promise ;
                var t2002;
                if(t2003 !== false) {
                 return ____25show(k1144, "#<promise>", t411);
                 t2002 = undefined;
                }
                else {
                 var t2005 = typeof(t414);
                 var t466 = t2005;
                 var t2006 = ("object") === (t466);
                 var t988 = t2006;
                 var t2007;
                 if(t988 !== false) {
                  return ____25print_2dhook(k1144, t414, t411, false);
                  t2007 = undefined;
                 }
                 else {
                  return ____25show(k1144, "#<unknown object>", t411);
                  t2007 = undefined;
                 }
                 t2002 = t2007;
                }
                t1998 = t2002;
               }
               t1978 = t1998;
              }
              t1975 = t1978;
             }
             t1945 = t1975;
            }
            t1941 = t1945;
           }
           t1936 = t1941;
          }
          t1932 = t1936;
         }
         t1928 = t1932;
        }
        t1924 = t1928;
       }
       t1920 = t1924;
      }
      t1907 = t1920;
     }
     t1903 = t1907;
    }
   };
   t413 = t1900;	// set! t413
   return t413(k1142, t409);
  };
  var t1157 = t1899;
  var t2011;
  if(t971 !== false) {
   return t1157(SPOCK.stdout);
   t2011 = undefined;
  }
  else {
   return ____25check_2dport(t1157, t410, "output", "display");
   t2011 = undefined;
  }
 };
 ___display = t1897;	// set! display
 var t467 = ___display;
 var t468 = undefined;
 var t2014 = function(K) {
  SPOCK.count(arguments, 't468');
  var str = arguments[ 1 ];
  var a = [];
  var len = str.length;
  for(var i = 0; i < len; ++i) {
   var c = str.charAt(i);
   switch(c) {
   case '\n': a.push('\n'); break;
   case '\t': a.push('\t'); break;
   case '\r': a.push('\r'); break;
   case '\"': a.push('\"'); break;
   case '\\': a.push('\\'); break;
   default: a.push(c);}}
  return K(a.join(''));
 };
 t468 = t2014;	// set! t468
 var t2015 = function (k1158, t469, t470) {
  var r = SPOCK.count(arguments);
  if(r) return r;
  var t2016 = t470 === undefined;
  var t989 = t2016;
  var t2017 = function (t1159) {	// t1177
   var t471 = t1159;
   var t473 = undefined;
   var t2018 = function (k1160, t474) {	// t473
    var r = SPOCK.count(arguments, "t473");
    if(r) return r;
    var t2019 = typeof(t474);
    var t477 = t2019;
    var t2020 = (t477) === ("string");
    var t476 = t2020;
    var t2021;
    if(t476 !== false) {
     t2021 = t476;
    }
    else {
     var t2022 = (t474) instanceof SPOCK.String ;
     t2021 = t2022;
    }
    var t990 = t2021;
    var t2023;
    if(t990 !== false) {
     var t2024 = function (t1161) {
      var t2025 = function (t1163) {
       var t2026 = function (t1162) {
        return ____25show(k1160, "\"", t471);
       };
       return ____25show(t2026, t1163, t471);
      };
      var t2029 = SPOCK.jstring(t474);
      return t468(t2025, t2029);
     };
     return ____25show(t2024, "\"", t471);
     t2023 = undefined;
    }
    else {
     var t2032 = (t474) instanceof SPOCK.Char ;
     var t991 = t2032;
     var t2033;
     if(t991 !== false) {
      var t2034 = function (t1164) {
       var t2035 = t474.character;
       var t480 = t2035;
       var t2037 = SPOCK.eqvp(t480, "\n");
       var t992 = t2037;
       var t2038;
       if(t992 !== false) {
        t2038 = true;
       }
       else {
        t2038 = false;
       }
       var t2036;
       if(t2038 !== false) {
        t2036 = "newline";
       }
       else {
        var t2040 = SPOCK.eqvp(t480, "\r");
        var t993 = t2040;
        var t2041;
        if(t993 !== false) {
         t2041 = true;
        }
        else {
         t2041 = false;
        }
        var t2039;
        if(t2041 !== false) {
         t2039 = "return";
        }
        else {
         var t2043 = SPOCK.eqvp(t480, "\t");
         var t994 = t2043;
         var t2044;
         if(t994 !== false) {
          t2044 = true;
         }
         else {
          t2044 = false;
         }
         var t2042;
         if(t2044 !== false) {
          t2042 = "tab";
         }
         else {
          var t2046 = SPOCK.eqvp(t480, " ");
          var t995 = t2046;
          var t2047;
          if(t995 !== false) {
           t2047 = true;
          }
          else {
           t2047 = false;
          }
          var t2045;
          if(t2047 !== false) {
           t2045 = "space";
          }
          else {
           t2045 = t480;
          }
          t2042 = t2045;
         }
         t2039 = t2042;
        }
        t2036 = t2039;
       }
       return ____25show(k1160, t2036, t471);
      };
      return ____25show(t2034, "#\\", t471);
      t2033 = undefined;
     }
     else {
      var t2050 = (t474) instanceof SPOCK.Pair ;
      var t996 = t2050;
      var t2051;
      if(t996 !== false) {
       var t2052 = function (t1165) {
        var t491 = undefined;
        var t2053 = function (k1166, t492) {	// t491
         var r = SPOCK.count(arguments, "t491");
         if(r) return r;
         var t2055 = null;
         var t2054 = (t492) === (t2055);
         var t997 = t2054;
         var t2056;
         if(t997 !== false) {
          return ____25show(k1166, ")", t471);
          t2056 = undefined;
         }
         else {
          var t2058 = (t492) instanceof SPOCK.Pair ;
          var t496 = t2058;
          var t2059;
          if(t496 !== false) {
           t2059 = false;
          }
          else {
           t2059 = true;
          }
          var t998 = t2059;
          var t2060;
          if(t998 !== false) {
           var t2061 = function (t1167) {
            var t2062 = function (t1168) {
             return ____25show(k1166, ")", t471);
            };
            return t473(t2062, t492);
           };
           return ____25show(t2061, " . ", t471);
           t2060 = undefined;
          }
          else {
           var t499 = t474;
           var t2066 = (t499) === (t492);
           var t498 = t2066;
           var t2067;
           if(t498 !== false) {
            t2067 = false;
           }
           else {
            t2067 = true;
           }
           var t999 = t2067;
           var t2068 = function (t1169) {	// t1171
            var t2069 = function (t1170) {
             var t2070 = SPOCK.check(t492, SPOCK.Pair);
             var t503 = t2070;
             var t2071 = t503.cdr;
             return t491(k1166, t2071);
            };
            var t2073 = t492.car;
            return t473(t2069, t2073);
           };
           var t1171 = t2068;
           var t2075;
           if(t999 !== false) {
            return ____25show(t1171, " ", t471);
            t2075 = undefined;
           }
           else {
            return t1171(undefined);
            t2075 = undefined;
           }
           t2060 = t2075;
          }
          t2056 = t2060;
         }
        };
        t491 = t2053;	// set! t491
        return t491(k1160, t474);
       };
       return ____25show(t2052, "(", t471);
       t2051 = undefined;
      }
      else {
       var t2080 = (t474) instanceof Array ;
       var t1000 = t2080;
       var t2081;
       if(t1000 !== false) {
        var t2082 = t474.length;
        var t505 = t2082;
        var t2083 = function (t1172) {
         var t506 = undefined;
         var t2084 = function (k1173, t507) {	// t506
          var r = SPOCK.count(arguments, "t506");
          if(r) return r;
          var t2086 = (t507) >= (t505);
          var t2085;
          if(t2086 !== false) {
           return ____25show(k1173, ")", t471);
           t2085 = undefined;
          }
          else {
           var t2088 = (t507) === (0);
           var t508 = t2088;
           var t2089;
           if(t508 !== false) {
            t2089 = false;
           }
           else {
            t2089 = true;
           }
           var t1001 = t2089;
           var t2090 = function (t1174) {	// t1176
            var t2091 = function (t1175) {
             var t2092 =  1+ (t507);
             return t506(k1173, t2092);
            };
            var t2094 = (t474) [ (t507) ] ;
            return t473(t2091, t2094);
           };
           var t1176 = t2090;
           var t2096;
           if(t1001 !== false) {
            return ____25show(t1176, " ", t471);
            t2096 = undefined;
           }
           else {
            return t1176(undefined);
            t2096 = undefined;
           }
           t2085 = t2096;
          }
         };
         t506 = t2084;	// set! t506
         return t506(k1160, 0);
        };
        return ____25show(t2083, "#(", t471);
        t2081 = undefined;
       }
       else {
        return t467(k1160, t474, t471);
        t2081 = undefined;
       }
       t2051 = t2081;
      }
      t2033 = t2051;
     }
     t2023 = t2033;
    }
   };
   t473 = t2018;	// set! t473
   return t473(k1158, t469);
  };
  var t1177 = t2017;
  var t2103;
  if(t989 !== false) {
   return t1177(SPOCK.stdout);
   t2103 = undefined;
  }
  else {
   return ____25check_2dport(t1177, t470, "output", "write");
   t2103 = undefined;
  }
 };
 ___write = t2015;	// set! write
 var t2106 = function(K) {
  SPOCK.count(arguments, 'apply');
  var proc = arguments[ 1 ];
  var argc = arguments.length;
  var lst = arguments[ argc - 1 ];
  var vec = [K].concat(Array.prototype.slice.call(arguments, 2, argc - 1));
  if(lst instanceof Array) vec = vec.concat(lst);
  else{
   var len = SPOCK.length(lst);
   var vec2 = new Array(len);
   for(var i = 0; lst instanceof SPOCK.Pair; lst = lst.cdr)
    vec2[ i++ ] = lst.car;
   vec = vec.concat(vec2);}
  return proc.apply(SPOCK.global, vec);
 };
 ___apply = t2106;	// set! apply
 var t2107 = function (k1178, t511, t512, t513) {	// for-each
  var r = SPOCK.count(arguments, "for-each");
  if(r) return r;
  var t513 = SPOCK.rest(arguments, 3, 'for-each');
  var t2109 = null;
  var t2108 = (t513) === (t2109);
  var t1002 = t2108;
  var t2110;
  if(t1002 !== false) {
   var t2111 = (t512) instanceof Array ;
   var t1003 = t2111;
   var t2112;
   if(t1003 !== false) {
    var t2114 = SPOCK.check(t512, Array, "for-each");
    var t2113 = t2114.length;
    var t518 = t2113;
    var t520 = undefined;
    var t2115 = function (k1179, t521) {	// t520
     var r = SPOCK.count(arguments, "t520");
     if(r) return r;
     var t523 = t518;
     var t2117 = SPOCK.check(t521, 'number', "t520");
     var t2118 = SPOCK.check(t523, 'number', "t520");
     var t2116 = (t2117)  >=  (t2118);
     var t1004 = t2116;
     var t2119;
     if(t1004 !== false) {
      return k1179(false);
      t2119 = undefined;
     }
     else {
      var t2121 = function (t1180) {
       var t2123 = SPOCK.check(t521, 'number');
       var t2124 = SPOCK.check(1, 'number');
       var t2122 = (t2123)  +  (t2124);
       return t520(k1179, t2122);
      };
      var t524 = t512;
      var t2127 = SPOCK.check(t524, Array, "t520");
      var t2128 = SPOCK.check(t521, 'number', "t520");
      var t2126 = (t2127) [ (t2128) ] ;
      return t511(t2121, t2126);
      t2119 = undefined;
     }
    };
    t520 = t2115;	// set! t520
    return t520(k1178, 0);
    t2112 = undefined;
   }
   else {
    var t528 = undefined;
    var t2131 = function (k1181, t529) {	// t528
     var r = SPOCK.count(arguments, "t528");
     if(r) return r;
     var t2132 = (t529) instanceof SPOCK.Pair ;
     var t1005 = t2132;
     var t2133;
     if(t1005 !== false) {
      var t2134 = function (t1182) {
       var t2135 = t529.cdr;
       return t528(k1181, t2135);
      };
      var t2137 = t529.car;
      return t511(t2134, t2137);
      t2133 = undefined;
     }
     else {
      return k1181(undefined);
      t2133 = undefined;
     }
    };
    t528 = t2131;	// set! t528
    return t528(k1178, t512);
    t2112 = undefined;
   }
   t2110 = t2112;
  }
  else {
   var t533 = undefined;
   var t2141 = function (k1183, t534) {	// t533
    var r = SPOCK.count(arguments, "t533");
    if(r) return r;
    var t536 = undefined;
    var t2142 = function (k1189, t537) {	// t536
     var r = SPOCK.count(arguments, "t536");
     if(r) return r;
     var t2144 = null;
     var t2143 = (t537) === (t2144);
     var t1006 = t2143;
     var t2145;
     if(t1006 !== false) {
      var t2146 = null;
      return k1189(t2146);
      t2145 = undefined;
     }
     else {
      var t2148 = t537.car;
      var t541 = t2148;
      var t2149 = (t541) instanceof SPOCK.Pair ;
      var t1007 = t2149;
      var t2150;
      if(t1007 !== false) {
       var t2151 = t541.car;
       var t544 = t2151;
       var t2152 = function (t1190) {
        var t545 = t1190;
        var t2153 = new SPOCK.Pair(t544, t545);
        return k1189(t2153);
       };
       var t2155 = t537.cdr;
       return t536(t2152, t2155);
       t2150 = undefined;
      }
      else {
       return k1189(false);
       t2150 = undefined;
      }
      t2145 = t2150;
     }
    };
    t536 = t2142;	// set! t536
    var t2158 = function (t1184) {
     var t535 = t1184;
     var t2159;
     if(t535 !== false) {
      var t2160 = function (t1185) {
       var t548 = undefined;
       var t2161 = function (k1187, t549) {	// t548
        var r = SPOCK.count(arguments, "t548");
        if(r) return r;
        var t2163 = null;
        var t2162 = (t549) === (t2163);
        var t1008 = t2162;
        var t2164;
        if(t1008 !== false) {
         var t2165 = null;
         return k1187(t2165);
         t2164 = undefined;
        }
        else {
         var t2167 = t549.car;
         var t555 = t2167;
         var t2168 = t555.cdr;
         var t553 = t2168;
         var t2169 = function (t1188) {
          var t554 = t1188;
          var t2170 = new SPOCK.Pair(t553, t554);
          return k1187(t2170);
         };
         var t2172 = t549.cdr;
         return t548(t2169, t2172);
         t2164 = undefined;
        }
       };
       t548 = t2161;	// set! t548
       var t2174 = function (t1186) {
        return t533(k1183, t1186);
       };
       return t548(t2174, t534);
      };
      return ___apply(t2160, t511, t535);
      t2159 = undefined;
     }
     else {
      return k1183(undefined);
      t2159 = undefined;
     }
    };
    return t536(t2158, t534);
   };
   t533 = t2141;	// set! t533
   var t2180 = new SPOCK.Pair(t512, t513);
   return t533(k1178, t2180);
   t2110 = undefined;
  }
 };
 ___for_2deach = t2107;	// set! for-each
 var t2182 = function (k1191, t560, t561, t562) {	// map
  var r = SPOCK.count(arguments, "map");
  if(r) return r;
  var t562 = SPOCK.rest(arguments, 3, 'map');
  var t2184 = null;
  var t2183 = (t562) === (t2184);
  var t1009 = t2183;
  var t2185;
  if(t1009 !== false) {
   var t2186 = (t561) instanceof Array ;
   var t1010 = t2186;
   var t2187;
   if(t1010 !== false) {
    var t2189 = SPOCK.check(t561, Array, "map");
    var t2188 = t2189.length;
    var t567 = t2188;
    var t2190 = function (t1192) {
     var t569 = t1192;
     var t570 = undefined;
     var t2191 = function (k1193, t571) {	// t570
      var r = SPOCK.count(arguments, "t570");
      if(r) return r;
      var t573 = t567;
      var t2193 = SPOCK.check(t571, 'number', "t570");
      var t2194 = SPOCK.check(t573, 'number', "t570");
      var t2192 = (t2193)  >=  (t2194);
      var t1011 = t2192;
      var t2195;
      if(t1011 !== false) {
       return k1193(t569);
       t2195 = undefined;
      }
      else {
       var t574 = t569;
       var t575 = t571;
       var t2197 = function (t1195) {
        var t576 = t1195;
        var t2199 = SPOCK.check(t574, Array);
        var t2200 = SPOCK.check(t575, 'number');
        var t2198 = (t2199) [ (t2200) ] =  (t576);
        var t1194 = t2198;
        var t2202 = SPOCK.check(t571, 'number');
        var t2203 = SPOCK.check(1, 'number');
        var t2201 = (t2202)  +  (t2203);
        return t570(k1193, t2201);
       };
       var t577 = t561;
       var t2206 = SPOCK.check(t577, Array, "t570");
       var t2207 = SPOCK.check(t571, 'number', "t570");
       var t2205 = (t2206) [ (t2207) ] ;
       return t560(t2197, t2205);
       t2195 = undefined;
      }
     };
     t570 = t2191;	// set! t570
     return t570(k1191, 0);
    };
    return ___make_2dvector(t2190, t567);
    t2187 = undefined;
   }
   else {
    var t581 = undefined;
    var t2211 = function (k1196, t582) {	// t581
     var r = SPOCK.count(arguments, "t581");
     if(r) return r;
     var t2212 = (t582) instanceof SPOCK.Pair ;
     var t1012 = t2212;
     var t2213;
     if(t1012 !== false) {
      var t2214 = function (t1197) {
       var t584 = t1197;
       var t2215 = function (t1198) {
        var t585 = t1198;
        var t2216 = new SPOCK.Pair(t584, t585);
        return k1196(t2216);
       };
       var t2218 = t582.cdr;
       return t581(t2215, t2218);
      };
      var t2220 = t582.car;
      return t560(t2214, t2220);
      t2213 = undefined;
     }
     else {
      var t2222 = null;
      return k1196(t2222);
      t2213 = undefined;
     }
    };
    t581 = t2211;	// set! t581
    return t581(k1191, t561);
    t2187 = undefined;
   }
   t2185 = t2187;
  }
  else {
   var t588 = undefined;
   var t2225 = function (k1199, t589) {	// t588
    var r = SPOCK.count(arguments, "t588");
    if(r) return r;
    var t591 = undefined;
    var t2226 = function (k1206, t592) {	// t591
     var r = SPOCK.count(arguments, "t591");
     if(r) return r;
     var t2228 = null;
     var t2227 = (t592) === (t2228);
     var t1013 = t2227;
     var t2229;
     if(t1013 !== false) {
      var t2230 = null;
      return k1206(t2230);
      t2229 = undefined;
     }
     else {
      var t2232 = t592.car;
      var t596 = t2232;
      var t2233 = (t596) instanceof SPOCK.Pair ;
      var t1014 = t2233;
      var t2234;
      if(t1014 !== false) {
       var t2235 = t596.car;
       var t599 = t2235;
       var t2236 = function (t1207) {
        var t600 = t1207;
        var t2237 = new SPOCK.Pair(t599, t600);
        return k1206(t2237);
       };
       var t2239 = t592.cdr;
       return t591(t2236, t2239);
       t2234 = undefined;
      }
      else {
       return k1206(false);
       t2234 = undefined;
      }
      t2229 = t2234;
     }
    };
    t591 = t2226;	// set! t591
    var t2242 = function (t1200) {
     var t590 = t1200;
     var t2243;
     if(t590 !== false) {
      var t2244 = function (t1201) {
       var t603 = t1201;
       var t605 = undefined;
       var t2245 = function (k1204, t606) {	// t605
        var r = SPOCK.count(arguments, "t605");
        if(r) return r;
        var t2247 = null;
        var t2246 = (t606) === (t2247);
        var t1015 = t2246;
        var t2248;
        if(t1015 !== false) {
         var t2249 = null;
         return k1204(t2249);
         t2248 = undefined;
        }
        else {
         var t2251 = t606.car;
         var t612 = t2251;
         var t2252 = t612.cdr;
         var t610 = t2252;
         var t2253 = function (t1205) {
          var t611 = t1205;
          var t2254 = new SPOCK.Pair(t610, t611);
          return k1204(t2254);
         };
         var t2256 = t606.cdr;
         return t605(t2253, t2256);
         t2248 = undefined;
        }
       };
       t605 = t2245;	// set! t605
       var t2258 = function (t1203) {
        var t2259 = function (t1202) {
         var t604 = t1202;
         var t2260 = new SPOCK.Pair(t603, t604);
         return k1199(t2260);
        };
        return t588(t2259, t1203);
       };
       return t605(t2258, t589);
      };
      return ___apply(t2244, t560, t590);
      t2243 = undefined;
     }
     else {
      var t2265 = null;
      return k1199(t2265);
      t2243 = undefined;
     }
    };
    return t591(t2242, t589);
   };
   t588 = t2225;	// set! t588
   var t2268 = new SPOCK.Pair(t561, t562);
   return t588(k1191, t2268);
   t2185 = undefined;
  }
 };
 ___map = t2182;	// set! map
 var t2270 = function (k1208, t619, t620, t621) {
  var r = SPOCK.count(arguments);
  if(r) return r;
  var t2271 = function (t1209) {
   var t2272 = new SPOCK.Pair(t619, t621);
   var t622 = t2272;
   var t623 = SPOCK.dynwinds;
   var t2273 = new SPOCK.Pair(t622, t623);
   SPOCK.dynwinds = t2273
   var t2274 = function (k1210) {
    var t626 = SPOCK.dynwinds;
    var t2275 = t626.cdr;
    SPOCK.dynwinds = t2275
    return t621(k1210);
   };
   return ____25call_2dwith_2dsaved_2dvalues(k1208, t620, t2274);
  };
  return t619(t2271);
 };
 ___dynamic_2dwind = t2270;	// set! dynamic-wind
 var t2279 = function(K) {
  SPOCK.count(arguments, '%call-with-current-continuation');
  var proc = arguments[ 1 ];
  function cont() {
   return K.apply(SPOCK.global, Array.prototype.slice.call(arguments, 1));}
  return proc(K, cont);
 };
 ____25call_2dwith_2dcurrent_2dcontinuation = t2279;	// set! %call-with-current-continuation
 var t627 = undefined;
 var t2280 = function (k1211, t628, t629) {	// t627
  var r = SPOCK.count(arguments, "t627");
  if(r) return r;
  var t631 = SPOCK.dynwinds;
  var t2281 = (t631) === (t628);
  var t630 = t2281;
  var t2282;
  if(t630 !== false) {
   return k1211(t630);
   t2282 = undefined;
  }
  else {
   var t2285 = SPOCK.check(t629, 'number', "t627");
   var t2286 = SPOCK.check(0, 'number', "t627");
   var t2284 = (t2285)  <  (t2286);
   var t1016 = t2284;
   var t2287;
   if(t1016 !== false) {
    var t2288 = function (t1212) {
     var t2289 = t628.car;
     var t636 = t2289;
     var t2290 = t636.car;
     var t2291 = function (t1213) {
      SPOCK.dynwinds = t628
      return k1211(undefined);
     };
     return t2290(t2291);
    };
    var t2294 = t628.cdr;
    var t2295 = (t629)  + 1 ;
    return t627(t2288, t2294, t2295);
    t2287 = undefined;
   }
   else {
    var t640 = SPOCK.dynwinds;
    var t2297 = t640.car;
    var t639 = t2297;
    var t2298 = t639.cdr;
    var t638 = t2298;
    var t641 = SPOCK.dynwinds;
    var t2299 = t641.cdr;
    SPOCK.dynwinds = t2299
    var t2300 = function (t1214) {
     var t2301 = (t629)  - 1 ;
     return t627(k1211, t628, t2301);
    };
    return t638(t2300);
    t2287 = undefined;
   }
   t2282 = t2287;
  }
 };
 t627 = t2280;	// set! t627
 var t2304 = function (k1215, t642) {
  var r = SPOCK.count(arguments);
  if(r) return r;
  var t643 = SPOCK.dynwinds;
  var t2305 = function (k1216, t644) {
   var r = SPOCK.count(arguments);
   if(r) return r;
   var t2306 = function (k1217, t645) {
    var t645 = SPOCK.rest(arguments, 1);
    var t646 = SPOCK.dynwinds;
    var t649 = t643;
    var t2307 = (t646) === (t649);
    var t647 = t2307;
    var t2308;
    if(t647 !== false) {
     t2308 = false;
    }
    else {
     t2308 = true;
    }
    var t1017 = t2308;
    var t2309 = function (t1218) {	// t1219
     return ___apply(k1217, t644, t645);
    };
    var t1219 = t2309;
    var t2311;
    if(t1017 !== false) {
     var t2312 = SPOCK.length(t646);
     var t650 = t2312;
     var t653 = t643;
     var t2313 = SPOCK.length(t653);
     var t651 = t2313;
     var t2315 = SPOCK.check(t650, 'number');
     var t2316 = SPOCK.check(t651, 'number');
     var t2314 = (t2315)  -  (t2316);
     return t627(t1219, t643, t2314);
     t2311 = undefined;
    }
    else {
     return t1219(undefined);
     t2311 = undefined;
    }
   };
   return t642(k1216, t2306);
  };
  return ____25call_2dwith_2dcurrent_2dcontinuation(k1215, t2305);
 };
 ___call_2dwith_2dcurrent_2dcontinuation = t2304;	// set! call-with-current-continuation
 var t2321 = function (k1220, t654) {	// %get-context
  var r = SPOCK.count(arguments, "%get-context");
  if(r) return r;
  return ___vector(k1220, t654, SPOCK.dynwinds, SPOCK.stdin, SPOCK.stdout, SPOCK.stderr);
 };
 ____25get_2dcontext = t2321;	// set! %get-context
 var t2323 = function(K) {
  SPOCK.count(arguments, '%restore-context');
  var state = arguments[ 1 ];
  SPOCK.dynwinds = state[ 1 ];
  SPOCK.stdin = state[ 2 ];
  SPOCK.stdout = state[ 3 ];
  SPOCK.stderr = state[ 4 ];
  return (state[ 0 ])(undefined);
 };
 ____25restore_2dcontext = t2323;	// set! %restore-context
 var t2324 = function (k1221, t655) {	// suspend
  var r = SPOCK.count(arguments, "suspend");
  if(r) return r;
  var t2325 = function (k1222, t656) {
   var r = SPOCK.count(arguments);
   if(r) return r;
   var t2326 = function (t1224) {
    var t2327 = function (t1223) {
     var t2328 = function(K) {
      SPOCK.count(arguments);
      return new SPOCK.Result(undefined);
     };
     return t2328(k1222);
    };
    return t655(t2327, t1224);
   };
   return ____25get_2dcontext(t2326, t656);
  };
  return ____25call_2dwith_2dcurrent_2dcontinuation(k1221, t2325);
 };
 ___suspend = t2324;	// set! suspend
 var t2333 = function (k1225, t657) {	// %make-promise
  var r = SPOCK.count(arguments, "%make-promise");
  if(r) return r;
  var t658 = false;
  var t659 = false;
  var t2335 = function (k1226) {	// t659
   var t2336;
   if(t658 !== false) {
    return ___apply(k1226, ___values, t659);
    t2336 = undefined;
   }
   else {
    var t2338 = function (k1227, t660) {
     var t660 = SPOCK.rest(arguments, 1);
     var t2339;
     if(t658 !== false) {
      return ___apply(k1227, ___values, t659);
      t2339 = undefined;
     }
     else {
      t658 = true;	// set! t658
      t659 = t660;	// set! t659
      return ___apply(k1227, ___values, t659);
      t2339 = undefined;
     }
    };
    return ___call_2dwith_2dvalues(k1226, t657, t2338);
    t2336 = undefined;
   }
  };
  var t2334 = new SPOCK.Promise(t2335);
  return k1225(t2334);
 };
 ____25make_2dpromise = t2333;	// set! %make-promise
 var t2344 = function (k1228, t661) {	// force
  var r = SPOCK.count(arguments, "force");
  if(r) return r;
  var t2346 = (t661)  instanceof SPOCK.Promise ;
  var t2345;
  if(t2346 !== false) {
   var t2347 = t661.thunk;
   return t2347(k1228);
   t2345 = undefined;
  }
  else {
   return k1228(t661);
   t2345 = undefined;
  }
 };
 ___force = t2344;	// set! force
 var t662 = ___dynamic_2dwind;
 var t2350 = function (k1229, t663, t664) {	// t662
  var r = SPOCK.count(arguments, "t662");
  if(r) return r;
  var t2351 = function (t1230) {
   var t665 = false;
   var t2352 = function (k1231) {
    loop: while(true) {
     t665 = SPOCK.stdin;	// set! t665
     SPOCK.stdin = t663
     return k1231(undefined);
    }
   };
   var t2354 = function (k1232) {
    loop: while(true) {
     SPOCK.stdin = t665
     return k1232(undefined);
    }
   };
   return t662(k1229, t2352, t664, t2354);
  };
  return ____25check_2dport(t2351, t663, "input", "with-input-from-port");
 };
 ___with_2dinput_2dfrom_2dport = t2350;	// set! with-input-from-port
 var t666 = ___dynamic_2dwind;
 var t2358 = function (k1233, t667, t668) {	// t666
  var r = SPOCK.count(arguments, "t666");
  if(r) return r;
  var t2359 = function (t1234) {
   var t669 = false;
   var t2360 = function (k1235) {
    loop: while(true) {
     t669 = SPOCK.stdout;	// set! t669
     SPOCK.stdout = t667
     return k1235(undefined);
    }
   };
   var t2362 = function (k1236) {
    loop: while(true) {
     SPOCK.stdout = t669
     return k1236(undefined);
    }
   };
   return t666(k1233, t2360, t668, t2362);
  };
  return ____25check_2dport(t2359, t667, "output", "with-output-to-port");
 };
 ___with_2doutput_2dto_2dport = t2358;	// set! with-output-to-port
 var t2366 = function(K) {
  SPOCK.count(arguments, '%close-port');
  var port = arguments[ 1 ];
  port.close();
  port.closed = true;
  return K(port);
 };
 ____25close_2dport = t2366;	// set! %close-port
 var t2367 = function(K) {
  SPOCK.count(arguments, 'open-input-file');
  var fn = SPOCK.check(arguments[ 1 ], 'string', 'open-input-file');
  return K(SPOCK.openInputFile(fn));
 };
 ___open_2dinput_2dfile = t2367;	// set! open-input-file
 var t2368 = function(K) {
  SPOCK.count(arguments, 'open-output-file');
  var fn = SPOCK.check(arguments[ 1 ], 'string', 'open-input-file');
  var exp = null;
  if(arguments.length === 3)
   exp = SPOCK.check(arguments[ 2 ], 'number', 'open-input-file');
  return K(SPOCK.openOutputFile(fn, exp));
 };
 ___open_2doutput_2dfile = t2368;	// set! open-output-file
 var t2369 = function (k1237, t670) {	// close-input-port
  var r = SPOCK.count(arguments, "close-input-port");
  if(r) return r;
  var t2370 = function (t1238) {
   var t671 = t1238;
   return ____25close_2dport(k1237, t671);
  };
  return ____25check_2dport(t2370, t670, "input", "close-input-port");
 };
 ___close_2dinput_2dport = t2369;	// set! close-input-port
 var t2373 = function (k1239, t672) {	// close-output-port
  var r = SPOCK.count(arguments, "close-output-port");
  if(r) return r;
  var t2374 = function (t1240) {
   var t673 = t1240;
   return ____25close_2dport(k1239, t673);
  };
  return ____25check_2dport(t2374, t672, "output", "close-output-port");
 };
 ___close_2doutput_2dport = t2373;	// set! close-output-port
 var t675 = ___open_2dinput_2dfile;
 var t2377 = function (k1241, t678, t679) {
  var r = SPOCK.count(arguments);
  if(r) return r;
  var t2378 = function (t1242) {
   var t680 = t1242;
   var t2379 = function (k1243) {
    return t679(k1243, t680);
   };
   var t2381 = function (k1244) {
    return ___close_2dinput_2dport(k1244, t680);
   };
   return ____25call_2dwith_2dsaved_2dvalues(k1241, t2379, t2381);
  };
  return t675(t2378, t678);
 };
 ___call_2dwith_2dinput_2dfile = t2377;	// set! call-with-input-file
 var t682 = ___open_2doutput_2dfile;
 var t2385 = function (k1245, t685, t686) {
  var r = SPOCK.count(arguments);
  if(r) return r;
  var t2386 = function (t1246) {
   var t687 = t1246;
   var t2387 = function (k1247) {
    return t686(k1247, t687);
   };
   var t2389 = function (k1248) {
    return ___close_2doutput_2dport(k1248, t687);
   };
   return ____25call_2dwith_2dsaved_2dvalues(k1245, t2387, t2389);
  };
  return t682(t2386, t685);
 };
 ___call_2dwith_2doutput_2dfile = t2385;	// set! call-with-output-file
 var t688 = ___with_2dinput_2dfrom_2dport;
 var t689 = ___open_2dinput_2dfile;
 var t693 = ___close_2dinput_2dport;
 var t2393 = function (k1249, t694, t695) {	// t693
  var r = SPOCK.count(arguments, "t693");
  if(r) return r;
  var t2394 = function (t1250) {
   var t696 = t1250;
   var t2395 = function (k1251) {
    var t2396 = function (k1252) {
     return t693(k1252, t696);
    };
    return ____25call_2dwith_2dsaved_2dvalues(k1251, t695, t2396);
   };
   return t688(k1249, t696, t2395);
  };
  return t689(t2394, t694);
 };
 ___with_2dinput_2dfrom_2dfile = t2393;	// set! with-input-from-file
 var t697 = ___with_2doutput_2dto_2dport;
 var t698 = ___open_2doutput_2dfile;
 var t702 = ___close_2doutput_2dport;
 var t2401 = function (k1253, t703, t704) {	// t702
  var r = SPOCK.count(arguments, "t702");
  if(r) return r;
  var t2402 = function (t1254) {
   var t705 = t1254;
   var t2403 = function (k1255) {
    var t2404 = function (k1256) {
     return t702(k1256, t705);
    };
    return ____25call_2dwith_2dsaved_2dvalues(k1255, t704, t2404);
   };
   return t697(k1253, t705, t2403);
  };
  return t698(t2402, t703);
 };
 ___with_2doutput_2dto_2dfile = t2401;	// set! with-output-to-file
 var t2409 = function (k1257, t706) {	// open-input-string
  var r = SPOCK.count(arguments, "open-input-string");
  if(r) return r;
  var t707 = undefined;
  var t2410 = function(K) {
   SPOCK.count(arguments, 't707');
   var buffer = arguments[ 1 ];
   var pos = 0;
   var len = buffer.length;
   function read(n) {
    if(pos >= len) return SPOCK.EOF;
    var str = buffer.substring(pos, pos + n);
    pos += n;
    return str;}
   return K(new SPOCK.Port('input', { read: read }));
  };
  t707 = t2410;	// set! t707
  var t2411 = SPOCK.jstring(t706);
  return t707(k1257, t2411);
 };
 ___open_2dinput_2dstring = t2409;	// set! open-input-string
 var t2413 = function(K) {
  SPOCK.count(arguments, 'open-output-string');
  var buffer = [];
  function write(s) { buffer.push(s); }
  var port = new SPOCK.Port('output', { write: write });
  port.buffer = buffer;
  port.isStringPort = true;
  return K(port);
 };
 ___open_2doutput_2dstring = t2413;	// set! open-output-string
 var t2414 = function (k1258, t709) {	// get-output-string
  var r = SPOCK.count(arguments, "get-output-string");
  if(r) return r;
  loop: while(true) {
   var t2415 = SPOCK.check(t709, SPOCK.Port, "get-output-string");
   var t710 = t2415;
   var t2416 = t710.isStringPort;
   var t712 = t2416;
   var t2417 = t712 === undefined;
   var t711 = t2417;
   var t2418;
   if(t711 !== false) {
    t2418 = false;
   }
   else {
    t2418 = true;
   }
   var t1018 = t2418;
   var t2419;
   if(t1018 !== false) {
    var t2421 = t710.buffer;
    var t2420 = t2421.join("");
    var t714 = t2420;
    var t2422 = new SPOCK.String(t714);
    var t713 = t2422;
    var t2423 = (t710) .buffer = [] ;
    t2419 = t713;
   }
   else {
    var t2424 = SPOCK.error("bad argument type - not a string port", t710);
    t2419 = t2424;
   }
   return k1258(t2419);
  }
 };
 ___get_2doutput_2dstring = t2414;	// set! get-output-string
 var t2426 = function (k1259, t715, t716) {	// with-input-from-string
  var r = SPOCK.count(arguments, "with-input-from-string");
  if(r) return r;
  var t2427 = function (t1260) {
   var t717 = t1260;
   return ___with_2dinput_2dfrom_2dport(k1259, t717, t716);
  };
  return ___open_2dinput_2dstring(t2427, t715);
 };
 ___with_2dinput_2dfrom_2dstring = t2426;	// set! with-input-from-string
 var t2430 = function (k1261, t718) {	// with-output-to-string
  var r = SPOCK.count(arguments, "with-output-to-string");
  if(r) return r;
  var t2431 = function (t1262) {
   var t719 = t1262;
   var t2432 = function (t1263) {
    return ___get_2doutput_2dstring(k1261, t719);
   };
   return ___with_2doutput_2dto_2dport(t2432, t719, t718);
  };
  return ___open_2doutput_2dstring(t2431);
 };
 ___with_2doutput_2dto_2dstring = t2430;	// set! with-output-to-string
 var t720 = ___read_2dchar;
 var t721 = ___reverse;
 var t722 = ___peek_2dchar;
 var t723 = ___list_2d_3evector;
 var t724 = ___list_2d_3estring;
 var t2436 = function (k1264) {	// t725
  loop: while(true) {
   return k1264(SPOCK.stdin);
  }
 };
 var t725 = t2436;
 var t726 = ___string_2d_3enumber;
 var t2438 = function (k1265, t727) {	// t726
  var r = SPOCK.count(arguments, "t726");
  if(r) return r;
  var t2439 = t727 === undefined;
  var t1019 = t2439;
  var t2440 = function (t1266) {	// t1323
   var t728 = t1266;
   var t730 = undefined;
   var t731 = undefined;
   var t732 = undefined;
   var t733 = undefined;
   var t734 = undefined;
   var t735 = undefined;
   var t736 = undefined;
   var t737 = undefined;
   var t2441 = function (k1267, t738) {	// t730
    var r = SPOCK.count(arguments, "t730");
    if(r) return r;
    var t2442 = function (t1268) {
     var t739 = t1268;
     var t2443;
     if(t739 !== false) {
      return k1267(t739);
      t2443 = undefined;
     }
     else {
      return ___string_2d_3esymbol(k1267, t738);
      t2443 = undefined;
     }
    };
    return t726(t2442, t738);
   };
   t730 = t2441;	// set! t730
   var t2447 = function (k1269) {	// t731
    var t2448 = function (t1270) {
     var t740 = t1270;
     var t743 = SPOCK.EOF;
     var t2449 = (t740) === (t743);
     var t1020 = t2449;
     var t2450;
     if(t1020 !== false) {
      return k1269(t740);
      t2450 = undefined;
     }
     else {
      var t744 = t740;
      var t2454 = new SPOCK.Char("#");
      var t2453 = SPOCK.eqvp(t740, t2454);
      var t1021 = t2453;
      var t2455;
      if(t1021 !== false) {
       t2455 = true;
      }
      else {
       t2455 = false;
      }
      var t2452;
      if(t2455 !== false) {
       return t734(k1269);
       t2452 = undefined;
      }
      else {
       var t2459 = new SPOCK.Char("(");
       var t2458 = SPOCK.eqvp(t744, t2459);
       var t1022 = t2458;
       var t2460;
       if(t1022 !== false) {
        t2460 = true;
       }
       else {
        t2460 = false;
       }
       var t2457;
       if(t2460 !== false) {
        var t2461 = new SPOCK.Char(")");
        return t735(k1269, t2461);
        t2457 = undefined;
       }
       else {
        var t2465 = new SPOCK.Char("[");
        var t2464 = SPOCK.eqvp(t744, t2465);
        var t1023 = t2464;
        var t2466;
        if(t1023 !== false) {
         t2466 = true;
        }
        else {
         t2466 = false;
        }
        var t2463;
        if(t2466 !== false) {
         var t2467 = new SPOCK.Char("]");
         return t735(k1269, t2467);
         t2463 = undefined;
        }
        else {
         var t2471 = new SPOCK.Char("{");
         var t2470 = SPOCK.eqvp(t744, t2471);
         var t1024 = t2470;
         var t2472;
         if(t1024 !== false) {
          t2472 = true;
         }
         else {
          t2472 = false;
         }
         var t2469;
         if(t2472 !== false) {
          var t2473 = new SPOCK.Char("}");
          return t735(k1269, t2473);
          t2469 = undefined;
         }
         else {
          var t2477 = new SPOCK.Char(",");
          var t2476 = SPOCK.eqvp(t744, t2477);
          var t1025 = t2476;
          var t2478;
          if(t1025 !== false) {
           t2478 = true;
          }
          else {
           t2478 = false;
          }
          var t2475;
          if(t2478 !== false) {
           var t2479 = function (t1271) {
            var t755 = t1271;
            var t2481 = new SPOCK.Char("@");
            var t2480 = SPOCK.eqvp(t755, t2481);
            var t1026 = t2480;
            var t2482;
            if(t1026 !== false) {
             var t2483 = function (t1273) {
              var t2484 = function (t1272) {
               var t757 = t1272;
               return k1269(t757);
              };
              var t2486 = SPOCK.intern("unquote-splicing");
              return ____25list(t2484, t2486, t1273);
             };
             return t731(t2483);
             t2482 = undefined;
            }
            else {
             var t2489 = function (t1275) {
              var t2490 = function (t1274) {
               var t758 = t1274;
               return k1269(t758);
              };
              var t2492 = SPOCK.intern("unquote");
              return ____25list(t2490, t2492, t1275);
             };
             return t731(t2489);
             t2482 = undefined;
            }
           };
           return t722(t2479, t728);
           t2475 = undefined;
          }
          else {
           var t2498 = new SPOCK.Char("`");
           var t2497 = SPOCK.eqvp(t744, t2498);
           var t1027 = t2497;
           var t2499;
           if(t1027 !== false) {
            t2499 = true;
           }
           else {
            t2499 = false;
           }
           var t2496;
           if(t2499 !== false) {
            var t2500 = function (t1277) {
             var t2501 = function (t1276) {
              var t761 = t1276;
              return k1269(t761);
             };
             var t2503 = SPOCK.intern("quasiquote");
             return ____25list(t2501, t2503, t1277);
            };
            return t731(t2500);
            t2496 = undefined;
           }
           else {
            var t2508 = new SPOCK.Char("'");
            var t2507 = SPOCK.eqvp(t744, t2508);
            var t1028 = t2507;
            var t2509;
            if(t1028 !== false) {
             t2509 = true;
            }
            else {
             t2509 = false;
            }
            var t2506;
            if(t2509 !== false) {
             var t2510 = function (t1279) {
              var t2511 = function (t1278) {
               var t764 = t1278;
               return k1269(t764);
              };
              var t2513 = SPOCK.intern("quote");
              return ____25list(t2511, t2513, t1279);
             };
             return t731(t2510);
             t2506 = undefined;
            }
            else {
             var t2518 = new SPOCK.Char(";");
             var t2517 = SPOCK.eqvp(t744, t2518);
             var t1029 = t2517;
             var t2519;
             if(t1029 !== false) {
              t2519 = true;
             }
             else {
              t2519 = false;
             }
             var t2516;
             if(t2519 !== false) {
              var t2520 = function (t1280) {
               return t731(k1269);
              };
              return t732(t2520);
              t2516 = undefined;
             }
             else {
              var t2525 = new SPOCK.Char("\"");
              var t2524 = SPOCK.eqvp(t744, t2525);
              var t1030 = t2524;
              var t2526;
              if(t1030 !== false) {
               t2526 = true;
              }
              else {
               t2526 = false;
              }
              var t2523;
              if(t2526 !== false) {
               return t736(k1269);
               t2523 = undefined;
              }
              else {
               var t2530 = new SPOCK.Char(")");
               var t2529 = SPOCK.eqvp(t744, t2530);
               var t1031 = t2529;
               var t2531;
               if(t1031 !== false) {
                t2531 = true;
               }
               else {
                var t2533 = new SPOCK.Char("]");
                var t2532 = SPOCK.eqvp(t744, t2533);
                var t1032 = t2532;
                var t2534;
                if(t1032 !== false) {
                 t2534 = true;
                }
                else {
                 var t2536 = new SPOCK.Char("}");
                 var t2535 = SPOCK.eqvp(t744, t2536);
                 var t1033 = t2535;
                 var t2537;
                 if(t1033 !== false) {
                  t2537 = true;
                 }
                 else {
                  t2537 = false;
                 }
                 t2534 = t2537;
                }
                t2531 = t2534;
               }
               var t2528;
               if(t2531 !== false) {
                return ____25error(k1269, "unexpected delimiter", t740);
                t2528 = undefined;
               }
               else {
                var t2540 = SPOCK.check(t740, SPOCK.Char);
                var t2539 = (t2540) .character.match(/^\s$/) ;
                var t777 = t2539;
                var t2542 = null;
                var t2541 = (t777) === (t2542);
                var t776 = t2541;
                var t2543;
                if(t776 !== false) {
                 t2543 = false;
                }
                else {
                 t2543 = true;
                }
                var t1034 = t2543;
                var t2544;
                if(t1034 !== false) {
                 return t731(k1269);
                 t2544 = undefined;
                }
                else {
                 var t2546 = function (t1283) {
                  var t780 = t1283;
                  var t1282 = t780;
                  var t2547 = function (t1281) {
                   return t730(k1269, t1281);
                  };
                  return t737(t2547, t1282);
                 };
                 return ____25list(t2546, t740);
                 t2544 = undefined;
                }
                t2528 = t2544;
               }
               t2523 = t2528;
              }
              t2516 = t2523;
             }
             t2506 = t2516;
            }
            t2496 = t2506;
           }
           t2475 = t2496;
          }
          t2469 = t2475;
         }
         t2463 = t2469;
        }
        t2457 = t2463;
       }
       t2452 = t2457;
      }
      t2450 = t2452;
     }
    };
    return t720(t2448, t728);
   };
   t731 = t2447;	// set! t731
   var t2552 = function (k1284) {	// t732
    var t2553 = function (t1285) {
     var t781 = t1285;
     var t786 = SPOCK.EOF;
     var t2554 = (t781) === (t786);
     var t783 = t2554;
     var t2555;
     if(t783 !== false) {
      t2555 = t783;
     }
     else {
      var t2558 = new SPOCK.Char("\n");
      var t2557 = SPOCK.check(t2558, SPOCK.Char);
      var t2556 = t2557.character;
      var t789 = t2556;
      var t2560 = SPOCK.check(t781, SPOCK.Char);
      var t2559 = t2560.character;
      var t790 = t2559;
      var t2561 = (t789) === (t790);
      t2555 = t2561;
     }
     var t782 = t2555;
     var t2562;
     if(t782 !== false) {
      t2562 = false;
     }
     else {
      t2562 = true;
     }
     var t1035 = t2562;
     var t2563;
     if(t1035 !== false) {
      return t732(k1284);
      t2563 = undefined;
     }
     else {
      return k1284(undefined);
      t2563 = undefined;
     }
    };
    return t720(t2553, t728);
   };
   t732 = t2552;	// set! t732
   var t2567 = function (k1286) {	// t733
    var t2568 = function (t1287) {
     var t791 = t1287;
     var t2570 = SPOCK.check(t791, SPOCK.Char);
     var t2569 = (t2570) .character.match(/^\s$/) ;
     var t794 = t2569;
     var t2572 = null;
     var t2571 = (t794) === (t2572);
     var t793 = t2571;
     var t2573;
     if(t793 !== false) {
      t2573 = false;
     }
     else {
      t2573 = true;
     }
     var t1036 = t2573;
     var t2574;
     if(t1036 !== false) {
      var t2575 = function (t1288) {
       return t733(k1286);
      };
      return t720(t2575, t728);
      t2574 = undefined;
     }
     else {
      return k1286(t791);
      t2574 = undefined;
     }
    };
    return t722(t2568, t728);
   };
   t733 = t2567;	// set! t733
   var t2580 = function (k1289) {	// t734
    var t2581 = function (t1290) {
     var t797 = t1290;
     var t800 = SPOCK.EOF;
     var t2582 = (t797) === (t800);
     var t1037 = t2582;
     var t2583;
     if(t1037 !== false) {
      return ____25error(k1289, "unexpected EOF after `#'");
      t2583 = undefined;
     }
     else {
      var t801 = t797;
      var t2587 = new SPOCK.Char("t");
      var t2586 = SPOCK.eqvp(t797, t2587);
      var t1038 = t2586;
      var t2588;
      if(t1038 !== false) {
       t2588 = true;
      }
      else {
       var t2590 = new SPOCK.Char("T");
       var t2589 = SPOCK.eqvp(t797, t2590);
       var t1039 = t2589;
       var t2591;
       if(t1039 !== false) {
        t2591 = true;
       }
       else {
        t2591 = false;
       }
       t2588 = t2591;
      }
      var t2585;
      if(t2588 !== false) {
       return k1289(true);
       t2585 = undefined;
      }
      else {
       var t2595 = new SPOCK.Char("f");
       var t2594 = SPOCK.eqvp(t797, t2595);
       var t1040 = t2594;
       var t2596;
       if(t1040 !== false) {
        t2596 = true;
       }
       else {
        var t2598 = new SPOCK.Char("F");
        var t2597 = SPOCK.eqvp(t797, t2598);
        var t1041 = t2597;
        var t2599;
        if(t1041 !== false) {
         t2599 = true;
        }
        else {
         t2599 = false;
        }
        t2596 = t2599;
       }
       var t2593;
       if(t2596 !== false) {
        return k1289(false);
        t2593 = undefined;
       }
       else {
        var t2603 = new SPOCK.Char("(");
        var t2602 = SPOCK.eqvp(t797, t2603);
        var t1042 = t2602;
        var t2604;
        if(t1042 !== false) {
         t2604 = true;
        }
        else {
         t2604 = false;
        }
        var t2601;
        if(t2604 !== false) {
         var t2605 = function (t1291) {
          return t723(k1289, t1291);
         };
         var t2607 = new SPOCK.Char(")");
         return t735(t2605, t2607);
         t2601 = undefined;
        }
        else {
         var t2611 = new SPOCK.Char("%");
         var t2610 = SPOCK.eqvp(t801, t2611);
         var t1043 = t2610;
         var t2612;
         if(t1043 !== false) {
          t2612 = true;
         }
         else {
          var t2614 = new SPOCK.Char("!");
          var t2613 = SPOCK.eqvp(t801, t2614);
          var t1044 = t2613;
          var t2615;
          if(t1044 !== false) {
           t2615 = true;
          }
          else {
           t2615 = false;
          }
          t2612 = t2615;
         }
         var t2609;
         if(t2612 !== false) {
          var t2616 = function (t1294) {
           var t816 = t1294;
           var t1293 = t816;
           var t2617 = function (t1292) {
            return ___string_2d_3esymbol(k1289, t1292);
           };
           return t737(t2617, t1293);
          };
          var t2620 = new SPOCK.Char("#");
          return ____25list(t2616, t797, t2620);
          t2609 = undefined;
         }
         else {
          var t2624 = new SPOCK.Char("\\");
          var t2623 = SPOCK.eqvp(t801, t2624);
          var t1045 = t2623;
          var t2625;
          if(t1045 !== false) {
           t2625 = true;
          }
          else {
           t2625 = false;
          }
          var t2622;
          if(t2625 !== false) {
           var t2626 = function (t1295) {
            var t819 = t1295;
            var t2628 = SPOCK.jstring("newline");
            var t2627 = t2628.toLowerCase();
            var t822 = t2627;
            var t2630 = SPOCK.jstring(t819);
            var t2629 = t2630.toLowerCase();
            var t823 = t2629;
            var t2631 = (t822) === (t823);
            var t1046 = t2631;
            var t2632;
            if(t1046 !== false) {
             var t2633 = new SPOCK.Char("\n");
             return k1289(t2633);
             t2632 = undefined;
            }
            else {
             var t2636 = SPOCK.jstring("tab");
             var t2635 = t2636.toLowerCase();
             var t828 = t2635;
             var t2638 = SPOCK.jstring(t819);
             var t2637 = t2638.toLowerCase();
             var t829 = t2637;
             var t2639 = (t828) === (t829);
             var t1047 = t2639;
             var t2640;
             if(t1047 !== false) {
              var t2641 = new SPOCK.Char("\t");
              return k1289(t2641);
              t2640 = undefined;
             }
             else {
              var t2644 = SPOCK.jstring("space");
              var t2643 = t2644.toLowerCase();
              var t834 = t2643;
              var t2646 = SPOCK.jstring(t819);
              var t2645 = t2646.toLowerCase();
              var t835 = t2645;
              var t2647 = (t834) === (t835);
              var t1048 = t2647;
              var t2648;
              if(t1048 !== false) {
               var t2649 = new SPOCK.Char(" ");
               return k1289(t2649);
               t2648 = undefined;
              }
              else {
               var t2652 = SPOCK.jstring(t819);
               var t2651 = t2652.length;
               var t838 = t2651;
               var t2653 = SPOCK.check(t838, 'number');
               var t842 = t2653;
               var t2654 = (0) === (t842);
               var t1049 = t2654;
               var t2655;
               if(t1049 !== false) {
                return ____25error(k1289, "invalid character syntax");
                t2655 = undefined;
               }
               else {
                return ___string_2dref(k1289, t819, 0);
                t2655 = undefined;
               }
               t2648 = t2655;
              }
              t2640 = t2648;
             }
             t2632 = t2640;
            }
           };
           var t2658 = null;
           return t737(t2626, t2658);
           t2622 = undefined;
          }
          else {
           return ____25error(k1289, "invalid `#' syntax", t797);
           t2622 = undefined;
          }
          t2609 = t2622;
         }
         t2601 = t2609;
        }
        t2593 = t2601;
       }
       t2585 = t2593;
      }
      t2583 = t2585;
     }
    };
    return t720(t2581, t728);
   };
   t734 = t2580;	// set! t734
   var t2662 = function (k1296, t843) {	// t735
    var r = SPOCK.count(arguments, "t735");
    if(r) return r;
    var t844 = undefined;
    var t2663 = function (k1297, t845) {	// t844
     var r = SPOCK.count(arguments, "t844");
     if(r) return r;
     var t2664 = function (t1298) {
      var t846 = t1298;
      var t849 = SPOCK.EOF;
      var t2665 = (t846) === (t849);
      var t1050 = t2665;
      var t2666;
      if(t1050 !== false) {
       return ____25error(k1297, "unexpected EOF while reading list");
       t2666 = undefined;
      }
      else {
       var t851 = t843;
       var t2669 = SPOCK.check(t846, SPOCK.Char);
       var t2668 = t2669.character;
       var t852 = t2668;
       var t2671 = SPOCK.check(t851, SPOCK.Char);
       var t2670 = t2671.character;
       var t853 = t2670;
       var t2672 = (t852) === (t853);
       var t1051 = t2672;
       var t2673;
       if(t1051 !== false) {
        var t2674 = function (t1299) {
         return t721(k1297, t845);
        };
        return t720(t2674, t728);
        t2673 = undefined;
       }
       else {
        var t2678 = new SPOCK.Char(".");
        var t2677 = SPOCK.eqvp(t2678, t846);
        var t1052 = t2677;
        var t2679;
        if(t1052 !== false) {
         var t2680 = function (t1300) {
          var t856 = t1300;
          var t2681 = SPOCK.jstring(".");
          var t859 = t2681;
          var t2682 = SPOCK.jstring(t856);
          var t860 = t2682;
          var t2683 = (t859) === (t860);
          var t1053 = t2683;
          var t2684;
          if(t1053 !== false) {
           var t2685 = function (t1301) {
            var t863 = t1301;
            var t2686 = function (t1302) {
             var t2687 = function (t1303) {
              var t864 = t1303;
              var t865 = t843;
              var t2688 = SPOCK.eqvp(t864, t865);
              var t1054 = t2688;
              var t2689;
              if(t1054 !== false) {
               var t2690 = function (t1304) {
                return ___append(k1297, t1304, t863);
               };
               return t721(t2690, t845);
               t2689 = undefined;
              }
              else {
               return ____25error(k1297, "missing closing delimiter", t843);
               t2689 = undefined;
              }
             };
             return t720(t2687, t728);
            };
            return t733(t2686);
           };
           return t731(t2685);
           t2684 = undefined;
          }
          else {
           var t2697 = function (t1306) {
            var t866 = t1306;
            var t2698 = new SPOCK.Pair(t866, undefined);
            var t1305 = t2698;
            return t844(k1297, t1305, t845);
           };
           return t730(t2697, t856);
           t2684 = undefined;
          }
         };
         var t2701 = null;
         return t737(t2680, t2701);
         t2679 = undefined;
        }
        else {
         var t2703 = function (t1308) {
          var t868 = t1308;
          var t2704 = new SPOCK.Pair(t868, t845);
          var t1307 = t2704;
          return t844(k1297, t1307);
         };
         return t731(t2703);
         t2679 = undefined;
        }
        t2673 = t2679;
       }
       t2666 = t2673;
      }
     };
     return t733(t2664);
    };
    t844 = t2663;	// set! t844
    var t2708 = null;
    return t844(k1296, t2708);
   };
   t735 = t2662;	// set! t735
   var t2710 = function (k1309) {	// t736
    var t870 = undefined;
    var t2711 = function (k1310, t871) {	// t870
     var r = SPOCK.count(arguments, "t870");
     if(r) return r;
     var t2712 = function (t1311) {
      var t872 = t1311;
      var t875 = SPOCK.EOF;
      var t2713 = (t872) === (t875);
      var t1055 = t2713;
      var t2714;
      if(t1055 !== false) {
       return ____25error(k1310, "unexpected EOF while reading string");
       t2714 = undefined;
      }
      else {
       var t2718 = new SPOCK.Char("\"");
       var t2717 = SPOCK.check(t2718, SPOCK.Char);
       var t2716 = t2717.character;
       var t878 = t2716;
       var t2720 = SPOCK.check(t872, SPOCK.Char);
       var t2719 = t2720.character;
       var t879 = t2719;
       var t2721 = (t878) === (t879);
       var t1056 = t2721;
       var t2722;
       if(t1056 !== false) {
        var t2723 = function (t1312) {
         return t724(k1310, t1312);
        };
        return t721(t2723, t871);
        t2722 = undefined;
       }
       else {
        var t2728 = new SPOCK.Char("\\");
        var t2727 = SPOCK.check(t2728, SPOCK.Char);
        var t2726 = t2727.character;
        var t882 = t2726;
        var t2730 = SPOCK.check(t872, SPOCK.Char);
        var t2729 = t2730.character;
        var t883 = t2729;
        var t2731 = (t882) === (t883);
        var t1057 = t2731;
        var t2732;
        if(t1057 !== false) {
         var t2733 = function (t1313) {
          var t884 = t1313;
          var t887 = SPOCK.EOF;
          var t2734 = (t884) === (t887);
          var t1058 = t2734;
          var t2735;
          if(t1058 !== false) {
           return ____25error(k1310, "unexpected EOF while reading string");
           t2735 = undefined;
          }
          else {
           var t2739 = new SPOCK.Char("n");
           var t2738 = SPOCK.eqvp(t884, t2739);
           var t1059 = t2738;
           var t2740;
           if(t1059 !== false) {
            t2740 = true;
           }
           else {
            t2740 = false;
           }
           var t2737;
           if(t2740 !== false) {
            var t2742 = new SPOCK.Char("\n");
            var t2741 = new SPOCK.Pair(t2742, t871);
            return t870(k1310, t2741);
            t2737 = undefined;
           }
           else {
            var t2746 = new SPOCK.Char("t");
            var t2745 = SPOCK.eqvp(t884, t2746);
            var t1060 = t2745;
            var t2747;
            if(t1060 !== false) {
             t2747 = true;
            }
            else {
             t2747 = false;
            }
            var t2744;
            if(t2747 !== false) {
             var t2749 = new SPOCK.Char("\t");
             var t2748 = new SPOCK.Pair(t2749, t871);
             return t870(k1310, t2748);
             t2744 = undefined;
            }
            else {
             var t2751 = new SPOCK.Pair(t884, t871);
             return t870(k1310, t2751);
             t2744 = undefined;
            }
            t2737 = t2744;
           }
           t2735 = t2737;
          }
         };
         return t720(t2733, t728);
         t2732 = undefined;
        }
        else {
         var t2754 = new SPOCK.Pair(t872, t871);
         return t870(k1310, t2754);
         t2732 = undefined;
        }
        t2722 = t2732;
       }
       t2714 = t2722;
      }
     };
     return t720(t2712, t728);
    };
    t870 = t2711;	// set! t870
    var t2757 = null;
    return t870(k1309, t2757);
   };
   t736 = t2710;	// set! t736
   var t2759 = function (k1314, t901) {	// t737
    var r = SPOCK.count(arguments, "t737");
    if(r) return r;
    var t902 = undefined;
    var t2760 = function (k1315, t903) {	// t902
     var r = SPOCK.count(arguments, "t902");
     if(r) return r;
     var t2761 = function (t1316) {
      var t904 = t1316;
      var t908 = SPOCK.EOF;
      var t2762 = (t904) === (t908);
      var t905 = t2762;
      var t2763 = function (t1317) {	// t1321
       var t1061 = t1317;
       var t2764;
       if(t1061 !== false) {
        var t2765 = function (t1318) {
         return t724(k1315, t1318);
        };
        return t721(t2765, t903);
        t2764 = undefined;
       }
       else {
        var t2768 = function (t1320) {
         var t915 = t1320;
         var t2769 = new SPOCK.Pair(t915, t903);
         var t1319 = t2769;
         return t902(k1315, t1319);
        };
        return t720(t2768, t728);
        t2764 = undefined;
       }
      };
      var t1321 = t2763;
      var t2772;
      if(t905 !== false) {
       return t1321(t905);
       t2772 = undefined;
      }
      else {
       var t2774 = function (t1322) {
        var t909 = t1322;
        var t2775;
        if(t909 !== false) {
         t2775 = t909;
        }
        else {
         var t2777 = SPOCK.check(t904, SPOCK.Char);
         var t2776 = (t2777) .character.match(/^\s$/) ;
         var t912 = t2776;
         var t2779 = null;
         var t2778 = (t912) === (t2779);
         var t911 = t2778;
         var t2780;
         if(t911 !== false) {
          t2780 = false;
         }
         else {
          t2780 = true;
         }
         t2775 = t2780;
        }
        return t1321(t2775);
       };
       var t2782 = new SPOCK.Pair(new SPOCK.Char("{"), new SPOCK.Pair(new SPOCK.Char("}"), new SPOCK.Pair(new SPOCK.Char("("), new SPOCK.Pair(new SPOCK.Char(")"), new SPOCK.Pair(new SPOCK.Char("["), new SPOCK.Pair(new SPOCK.Char("]"), new SPOCK.Pair(new SPOCK.Char(";"), new SPOCK.Pair(new SPOCK.Char("\""), null))))))));
       return ___memv(t2774, t904, t2782);
       t2772 = undefined;
      }
     };
     return t722(t2761, t728);
    };
    t902 = t2760;	// set! t902
    return t902(k1314, t901);
   };
   t737 = t2759;	// set! t737
   return t731(k1265);
  };
  var t1323 = t2440;
  var t2787;
  if(t1019 !== false) {
   return t725(t1323);
   t2787 = undefined;
  }
  else {
   return t1323(t727);
   t2787 = undefined;
  }
 };
 ___read = t2438;	// set! read
 var t2790 = function (k1324, t917, t918) {	// load
  var r = SPOCK.count(arguments, "load");
  if(r) return r;
  loop: while(true) {
   var t2792 = SPOCK.jstring(t917);
   var t2793 = t918 === undefined;
   var t920 = t2793;
   var t2794;
   if(t920 !== false) {
    t2794 = false;
   }
   else {
    t2794 = true;
   }
   var t1062 = t2794;
   var t2795;
   if(t1062 !== false) {
    var t2796 = SPOCK.callback(t918);
    t2795 = t2796;
   }
   else {
    t2795 = false;
   }
   var t2791 = SPOCK.load(t2792, t2795);
   return k1324(t2791);
  }
 };
 ___load = t2790;	// set! load
 var t2798 = function(K) {
  SPOCK.count(arguments, '%error');
  SPOCK.error.apply(SPOCK.global, Array.prototype.slice.call(arguments, 1));
 };
 ____25error = t2798;	// set! %error
 ___error = ____25error;	// set! error
 var t2799 = function (k1325, t922) {	// exit
  var r = SPOCK.count(arguments, "exit");
  if(r) return r;
  loop: while(true) {
   var t2801 = t922 === undefined;
   var t1063 = t2801;
   var t2802;
   if(t1063 !== false) {
    t2802 = 0;
   }
   else {
    var t2803 = SPOCK.check(t922, 'number', "exit");
    t2802 = t2803;
   }
   var t2800 = SPOCK.exit(t2802);
   return k1325(t2800);
  }
 };
 ___exit = t2799;	// set! exit
 var t2805 = function (k1326, t924) {	// milliseconds
  var r = SPOCK.count(arguments, "milliseconds");
  if(r) return r;
  var t2806 = (new Date()).getTime();
  var t925 = t2806;
  var t2807 = t924 === undefined;
  var t1064 = t2807;
  var t2808;
  if(t1064 !== false) {
   return k1326(t925);
   t2808 = undefined;
  }
  else {
   var t2810 = function (t1327) {
    var t2811 = (new Date()).getTime();
    var t928 = t2811;
    var t2812 = (t928) - (t925);
    return k1326(t2812);
   };
   return t924(t2810);
   t2808 = undefined;
  }
 };
 ___milliseconds = t2805;	// set! milliseconds
 var t2815 = function (k1328, t929) {	// print
  var t929 = SPOCK.rest(arguments, 1, 'print');
  var t2816 = function (t1329) {
   return ___newline(k1328);
  };
  return ___for_2deach(t2816, ___display, t929);
 };
 ___print = t2815;	// set! print
 var t2819 = function (k1330, t930) {	// o
  var t930 = SPOCK.rest(arguments, 1, 'o');
  var t2821 = null;
  var t2820 = (t930) === (t2821);
  var t1065 = t2820;
  var t2822;
  if(t1065 !== false) {
   var t2823 = function (k1331, t934) {
    var r = SPOCK.count(arguments);
    if(r) return r;
    loop: while(true) {
     return k1331(t934);
    }
   };
   return k1330(t2823);
   t2822 = undefined;
  }
  else {
   var t935 = undefined;
   var t2826 = function (k1332, t936) {	// t935
    var r = SPOCK.count(arguments, "t935");
    if(r) return r;
    var t2827 = t936.car;
    var t937 = t2827;
    var t2828 = t936.cdr;
    var t938 = t2828;
    var t2830 = null;
    var t2829 = (t938) === (t2830);
    var t1066 = t2829;
    var t2831;
    if(t1066 !== false) {
     t2831 = t937;
    }
    else {
     var t2832 = function (k1333, t944) {
      var r = SPOCK.count(arguments);
      if(r) return r;
      var t2833 = function (t1335) {
       var t2834 = function (t1334) {
        return t937(k1333, t1334);
       };
       return t1335(t2834, t944);
      };
      return t935(t2833, t938);
     };
     t2831 = t2832;
    }
    return k1332(t2831);
   };
   t935 = t2826;	// set! t935
   return t935(k1330, t930);
   t2822 = undefined;
  }
 };
 ___o = t2819;	// set! o
 var t2840 = function(K) {
  SPOCK.count(arguments, '%');
  var o = {};
  for(var i = 1; i < arguments.length; i += 2) {
   var x = arguments[ i ];
   if(typeof x === 'string') o[ x ] = arguments[ i + 1 ];
   else if(x instanceof SPOCK.String)
    o[ x.name ] = arguments[ i + 1 ];
   else SPOCK.error('(%) object key not a string or symbol', x);}
  return K(o);
 };
 ____25 = t2840;	// set! %
 var t2841 = function(K) {
  SPOCK.count(arguments, 'native');
  var func = arguments[ 1 ];
  return K(function(k) {
   var args = Array.prototype.splice.call(arguments, 1);
   return k(func.apply(SPOCK.global, args));});
 };
 ___native = t2841;	// set! native
 var t2842 = function(K) {
  SPOCK.count(arguments, 'native-method');
  var func = arguments[ 1 ];
  return K(function(k) {
   var args = Array.prototype.splice.call(arguments, 2);
   return k(func.apply(arguments[ 1 ], args));});
 };
 ___native_2dmethod = t2842;	// set! native-method
 var t2843 = function(K) {
  SPOCK.count(arguments, 'bind-method');
  var func = arguments[ 1 ];
  var that = arguments[ 2 ];
  return K(function() { return func.apply(that, arguments); });
 };
 ___bind_2dmethod = t2843;	// set! bind-method
 var t2844 = function(K) {
  SPOCK.count(arguments, 'jstring');
  var x = arguments[ 1 ];
  if(typeof x === 'string') return K(x);
  else if(x instanceof SPOCK.String) return K(x.normalize());
  else if(x instanceof SPOCK.Char) return K(x.character);
  else return K(x);
 };
 ___jstring = t2844;	// set! jstring
 return k1067(undefined);
};
SPOCK.run(t1336);
SPOCK.flush();
/* END OF GENERATED CODE */
