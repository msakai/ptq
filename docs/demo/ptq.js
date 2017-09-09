"use strict";
// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, obj];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1,_2){while(1){var _3=E(_2);if(!_3[0]){return false;}else{if(!B(A(_1,[_3[1]]))){_2=_3[2];continue;}else{return true;}}}},_4=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_5=new T(function(){return B(err(_4));}),_6=function(_7){var _8=E(_7);return _8[0]==0?E(_5):E(_8[1]);},_9=function(_a){return E(_a)[0]==0?true:false;},_b=[0],_c=function(_d,_){var _e=E(_d);if(!_e[0]){return _b;}else{var _f=jsFind(E(_e[1])[1]),_g=_f,_h=B(_c(_e[2],_)),_i=_h;return [1,_g,_i];}},_j=0,_k=new T(function(){return [0,"keypress"];}),_l=new T(function(){return [0,"click"];}),_m=new T(function(){return B(unCStr("Control.Exception.Base"));}),_n=new T(function(){return B(unCStr("base"));}),_o=new T(function(){return B(unCStr("PatternMatchFail"));}),_p=new T(function(){var _q=hs_wordToWord64(18445595),_r=_q,_s=hs_wordToWord64(52003073),_t=_s;return [0,_r,_t,[0,_r,_t,_n,_m,_o],_b];}),_u=function(_v){return E(_p);},_w=function(_x){return E(E(_x)[1]);},_y=function(_z,_A,_B){var _C=B(A(_z,[_])),_D=B(A(_A,[_])),_E=hs_eqWord64(_C[1],_D[1]),_F=_E;if(!E(_F)){return [0];}else{var _G=hs_eqWord64(_C[2],_D[2]),_H=_G;return E(_H)==0?[0]:[1,_B];}},_I=function(_J){var _K=E(_J);return new F(function(){return _y(B(_w(_K[1])),_u,_K[2]);});},_L=function(_M){return E(E(_M)[1]);},_N=function(_O,_P){var _Q=E(_O);return _Q[0]==0?E(_P):[1,_Q[1],new T(function(){return B(_N(_Q[2],_P));})];},_R=function(_S,_T){return new F(function(){return _N(E(_S)[1],_T);});},_U=[0,44],_V=[0,93],_W=[0,91],_X=function(_Y,_Z,_10){var _11=E(_Z);return _11[0]==0?B(unAppCStr("[]",_10)):[1,_W,new T(function(){return B(A(_Y,[_11[1],new T(function(){var _12=function(_13){var _14=E(_13);return _14[0]==0?E([1,_V,_10]):[1,_U,new T(function(){return B(A(_Y,[_14[1],new T(function(){return B(_12(_14[2]));})]));})];};return B(_12(_11[2]));})]));})];},_15=function(_16,_17){return new F(function(){return _X(_R,_16,_17);});},_18=function(_19,_1a,_1b){return new F(function(){return _N(E(_1a)[1],_1b);});},_1c=[0,_18,_L,_15],_1d=new T(function(){return [0,_u,_1c,_1e,_I];}),_1e=function(_1f){return [0,_1d,_1f];},_1g=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_1h=function(_1i,_1j){return new F(function(){return die(new T(function(){return B(A(_1j,[_1i]));}));});},_1k=function(_1l,_1m){var _1n=E(_1m);if(!_1n[0]){return [0,_b,_b];}else{var _1o=_1n[1];if(!B(A(_1l,[_1o]))){return [0,_b,_1n];}else{var _1p=new T(function(){var _1q=B(_1k(_1l,_1n[2]));return [0,_1q[1],_1q[2]];});return [0,[1,_1o,new T(function(){return E(E(_1p)[1]);})],new T(function(){return E(E(_1p)[2]);})];}}},_1r=[0,32],_1s=[0,10],_1t=[1,_1s,_b],_1u=function(_1v){return E(E(_1v)[1])==124?false:true;},_1w=function(_1x,_1y){var _1z=B(_1k(_1u,B(unCStr(_1x)))),_1A=_1z[1],_1B=function(_1C,_1D){return new F(function(){return _N(_1C,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_N(_1y,new T(function(){return B(_N(_1D,_1t));},1)));})));},1));});},_1E=E(_1z[2]);if(!_1E[0]){return new F(function(){return _1B(_1A,_b);});}else{return E(E(_1E[1])[1])==124?B(_1B(_1A,[1,_1r,_1E[2]])):B(_1B(_1A,_b));}},_1F=function(_1G){return new F(function(){return _1h([0,new T(function(){return B(_1w(_1G,_1g));})],_1e);});},_1H=function(_1I){return new F(function(){return _1F("ptq.hs:(20,98)-(33,13)|lambda");});},_1J=new T(function(){return B(_1H(_));}),_1K=new T(function(){return B(unCStr("value"));}),_1L=function(_1M){var _1N=E(_1M);if(!_1N[0]){return [0];}else{return new F(function(){return _N(_1N[1],new T(function(){return B(_1L(_1N[2]));},1));});}},_1O=[0,1],_1P=new T(function(){return B(unCStr(" // "));}),_1Q=new T(function(){return B(unCStr(" / "));}),_1R=new T(function(){return B(unCStr("Adj"));}),_1S=function(_1T){return new F(function(){return _N(_1R,_1T);});},_1U=new T(function(){return B(unCStr("CN"));}),_1V=function(_1T){return new F(function(){return _N(_1U,_1T);});},_1W=new T(function(){return B(unCStr("IV"));}),_1X=function(_1T){return new F(function(){return _N(_1W,_1T);});},_1Y=[0,116],_1Z=[1,_1Y,_b],_20=function(_1T){return new F(function(){return _N(_1Z,_1T);});},_21=[0,41],_22=[0,40],_23=function(_24,_25){var _26=E(_25);switch(_26[0]){case 0:return E(_20);case 1:return E(_1X);case 2:return E(_1V);case 3:return E(_1S);case 4:var _27=new T(function(){return B(_23(_1O,_26[3]));}),_28=new T(function(){return B(_23(_1O,_26[2]));});return E(_24)[1]<=0?function(_29){return new F(function(){return A(_28,[new T(function(){return B(_N(_1Q,new T(function(){return B(A(_27,[_29]));},1)));})]);});}:function(_2a){return [1,_22,new T(function(){return B(A(_28,[new T(function(){return B(_N(_1Q,new T(function(){return B(A(_27,[[1,_21,_2a]]));},1)));})]));})];};default:var _2b=new T(function(){return B(_23(_1O,_26[3]));}),_2c=new T(function(){return B(_23(_1O,_26[2]));});return E(_24)[1]<=0?function(_2d){return new F(function(){return A(_2c,[new T(function(){return B(_N(_1P,new T(function(){return B(A(_2b,[_2d]));},1)));})]);});}:function(_2e){return [1,_22,new T(function(){return B(A(_2c,[new T(function(){return B(_N(_1P,new T(function(){return B(A(_2b,[[1,_21,_2e]]));},1)));})]));})];};}},_2f=[0,0],_2g=function(_2h){return new F(function(){return A(_2i,[_2f,_2h,_b]);});},_2j=function(_1T){return new F(function(){return _2i(_2f,_1T);});},_2k=function(_2l,_2m){return new F(function(){return _X(_2j,_2l,_2m);});},_2n=new T(function(){return [0,_2i,_2g,_2k];}),_2o=[0,32],_2p=[0,11],_2q=function(_2r,_2s,_2t){var _2u=new T(function(){return B(_2i(_2p,_2t));});return _2r<=10?function(_2v){return new F(function(){return _N(_2s,[1,_2o,new T(function(){return B(A(_2u,[_2v]));})]);});}:function(_2w){return [1,_22,new T(function(){return B(_N(_2s,[1,_2o,new T(function(){return B(A(_2u,[[1,_21,_2w]]));})]));})];};},_2x=function(_2y,_2z,_2A){return new F(function(){return _2q(E(_2y)[1],_2z,_2A);});},_2B=function(_2C,_2D,_2E){var _2F=new T(function(){return B(_2i(_2p,_2E));});return _2C<=10?function(_2G){return new F(function(){return _N(_2D,[1,_2o,new T(function(){return B(A(_2F,[_2G]));})]);});}:function(_2H){return [1,_22,new T(function(){return B(_N(_2D,[1,_2o,new T(function(){return B(A(_2F,[[1,_21,_2H]]));})]));})];};},_2I=function(_2J,_2K,_2L){return new F(function(){return _2B(E(_2J)[1],_2K,_2L);});},_2M=function(_2N,_2O){var _2P=jsShowI(_2N),_2Q=_2P;return new F(function(){return _N(fromJSStr(_2Q),_2O);});},_2R=function(_2S,_2T,_2U){if(_2T>=0){return new F(function(){return _2M(_2T,_2U);});}else{return _2S<=6?B(_2M(_2T,_2U)):[1,_22,new T(function(){var _2V=jsShowI(_2T),_2W=_2V;return B(_N(fromJSStr(_2W),[1,_21,_2U]));})];}},_2X=function(_2Y,_2Z,_30,_31){return _2Y<=10?B(_N(_2Z,[1,_2o,new T(function(){return B(_2R(11,E(_30)[1],_31));})])):[1,_22,new T(function(){return B(_N(_2Z,[1,_2o,new T(function(){return B(_2R(11,E(_30)[1],[1,_21,_31]));})]));})];},_32=function(_33,_34,_35,_36){return new F(function(){return _2X(E(_33)[1],_34,_35,_36);});},_37=function(_38,_39,_3a,_3b){var _3c=function(_3d){return new F(function(){return _N(_39,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3a));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3b));}),[_3d]));})]]));})]);});};return _38<=10?E(_3c):function(_3e){return [1,_22,new T(function(){return B(_3c([1,_21,_3e]));})];};},_3f=function(_3g,_3h,_3i,_3j){return new F(function(){return _37(E(_3g)[1],_3h,_3i,_3j);});},_3k=function(_3l,_3m,_3n,_3o){var _3p=function(_3q){return new F(function(){return _N(_3m,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3n));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3o));}),[_3q]));})]]));})]);});};return _3l<=10?E(_3p):function(_3r){return [1,_22,new T(function(){return B(_3p([1,_21,_3r]));})];};},_3s=function(_3t,_3u,_3v,_3w){return new F(function(){return _3k(E(_3t)[1],_3u,_3v,_3w);});},_3x=function(_3y,_3z,_3A,_3B){var _3C=function(_3D){return new F(function(){return _N(_3z,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3A));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3B));}),[_3D]));})]]));})]);});};return _3y<=10?E(_3C):function(_3E){return [1,_22,new T(function(){return B(_3C([1,_21,_3E]));})];};},_3F=function(_3G,_3H,_3I,_3J){return new F(function(){return _3x(E(_3G)[1],_3H,_3I,_3J);});},_3K=function(_3L,_3M,_3N,_3O){var _3P=function(_3Q){return new F(function(){return _N(_3M,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3N));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_3O));}),[_3Q]));})]]));})]);});};return _3L<=10?E(_3P):function(_3R){return [1,_22,new T(function(){return B(_3P([1,_21,_3R]));})];};},_3S=function(_3T,_3U,_3V,_3W){return new F(function(){return _3K(E(_3T)[1],_3U,_3V,_3W);});},_3X=function(_3Y,_3Z,_40,_41){var _42=function(_43){return new F(function(){return _N(_3Z,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_40));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_41));}),[_43]));})]]));})]);});};return _3Y<=10?E(_42):function(_44){return [1,_22,new T(function(){return B(_42([1,_21,_44]));})];};},_45=function(_46,_47,_48,_49){return new F(function(){return _3X(E(_46)[1],_47,_48,_49);});},_4a=function(_4b,_4c,_4d,_4e){var _4f=function(_4g){return new F(function(){return _N(_4c,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_4d));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_4e));}),[_4g]));})]]));})]);});};return _4b<=10?E(_4f):function(_4h){return [1,_22,new T(function(){return B(_4f([1,_21,_4h]));})];};},_4i=function(_4j,_4k,_4l,_4m){return new F(function(){return _4a(E(_4j)[1],_4k,_4l,_4m);});},_4n=function(_4o,_4p,_4q,_4r){var _4s=function(_4t){return new F(function(){return _N(_4p,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_4q));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_4r));}),[_4t]));})]]));})]);});};return _4o<=10?E(_4s):function(_4u){return [1,_22,new T(function(){return B(_4s([1,_21,_4u]));})];};},_4v=function(_4w,_4x,_4y,_4z){return new F(function(){return _4n(E(_4w)[1],_4x,_4y,_4z);});},_4A=function(_4B,_4C,_4D,_4E){var _4F=function(_4G){return new F(function(){return _N(_4C,[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_4D));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_4E));}),[_4G]));})]]));})]);});};return _4B<=10?E(_4F):function(_4H){return [1,_22,new T(function(){return B(_4F([1,_21,_4H]));})];};},_4I=function(_4J,_4K,_4L,_4M){return new F(function(){return _4A(E(_4J)[1],_4K,_4L,_4M);});},_4N=function(_4O){return E(E(_4O)[1]);},_4P=function(_4Q,_4R,_4S,_4T,_4U,_4V){var _4W=function(_4X){return new F(function(){return _N(_4T,[1,_2o,new T(function(){return B(A(new T(function(){return B(A(_4N,[_4Q,_2p,_4U]));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(A(_4N,[_4R,_2p,_4V]));}),[_4X]));})]]));})]);});};return _4S<=10?E(_4W):function(_4Y){return [1,_22,new T(function(){return B(_4W([1,_21,_4Y]));})];};},_4Z=new T(function(){return B(unCStr("F25"));}),_50=new T(function(){return B(unCStr("F24"));}),_51=new T(function(){return B(unCStr("F14"));}),_52=new T(function(){return B(unCStr("F13"));}),_53=new T(function(){return B(unCStr("F12"));}),_54=new T(function(){return B(unCStr("F11"));}),_55=new T(function(){return B(unCStr("F10"));}),_56=new T(function(){return B(unCStr("F9"));}),_57=new T(function(){return B(unCStr("F8"));}),_58=new T(function(){return B(unCStr("F7"));}),_59=new T(function(){return B(unCStr("F6"));}),_5a=new T(function(){return B(unCStr("F5"));}),_5b=new T(function(){return B(unCStr("F23"));}),_5c=new T(function(){return B(unCStr("F4"));}),_5d=new T(function(){return B(unCStr("F3"));}),_5e=new T(function(){return B(unCStr("F2"));}),_5f=new T(function(){return B(unCStr("He"));}),_5g=new T(function(){return B(unCStr("F22"));}),_5h=new T(function(){return B(unCStr("F21"));}),_5i=new T(function(){return B(unCStr("F20"));}),_5j=new T(function(){return B(unCStr("F19"));}),_5k=new T(function(){return B(unCStr("F17"));}),_5l=new T(function(){return B(unCStr("F16"));}),_5m=new T(function(){return B(unCStr("F15"));}),_2i=function(_5n,_5o){var _5p=E(_5o);switch(_5p[0]){case 0:return function(_5q){return new F(function(){return _N(_5p[2],_5q);});};case 1:return function(_5q){return new F(function(){return _32(_5n,_5f,_5p[2],_5q);});};case 2:return new F(function(){return _3F(_5n,_5e,_5p[2],_5p[3]);});break;case 3:var _5r=function(_5s){return new F(function(){return _N(_5d,[1,_2o,new T(function(){return B(_2R(11,E(_5p[2])[1],[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_5p[3]));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_5p[4]));}),[_5s]));})]]));})]));})]);});};return E(_5n)[1]<=10?E(_5r):function(_5t){return [1,_22,new T(function(){return B(_5r([1,_21,_5t]));})];};case 4:return new F(function(){return _4v(_5n,_5c,_5p[2],_5p[3]);});break;case 5:return new F(function(){return _4P(_2n,_2n,E(_5n)[1],_5a,_5p[2],_5p[3]);});break;case 6:return new F(function(){return _4P(_2n,_2n,E(_5n)[1],_59,_5p[2],_5p[3]);});break;case 7:return new F(function(){return _45(_5n,_58,_5p[2],_5p[3]);});break;case 8:return new F(function(){return _4P(_2n,_2n,E(_5n)[1],_57,_5p[2],_5p[3]);});break;case 9:return new F(function(){return _4P(_2n,_2n,E(_5n)[1],_56,_5p[2],_5p[3]);});break;case 10:var _5u=function(_5v){return new F(function(){return _N(_55,[1,_2o,new T(function(){return B(_2R(11,E(_5p[2])[1],[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_5p[3]));}),[[1,_2o,new T(function(){return B(A(new T(function(){return B(_2i(_2p,_5p[4]));}),[_5v]));})]]));})]));})]);});};return E(_5n)[1]<=10?E(_5u):function(_5w){return [1,_22,new T(function(){return B(_5u([1,_21,_5w]));})];};case 11:return new F(function(){return _4v(_5n,_54,_5p[2],_5p[3]);});break;case 12:return new F(function(){return _4v(_5n,_53,_5p[2],_5p[3]);});break;case 13:return new F(function(){return _4v(_5n,_52,_5p[2],_5p[3]);});break;case 14:return new F(function(){return _4v(_5n,_51,_5p[2],_5p[3]);});break;case 15:return new F(function(){return _4v(_5n,_5m,_5p[2],_5p[3]);});break;case 16:return new F(function(){return _4i(_5n,_5l,_5p[2],_5p[3]);});break;case 17:return new F(function(){return _4I(_5n,_5k,_5p[2],_5p[3]);});break;case 18:return new F(function(){return _2I(_5n,_5j,_5p[2]);});break;case 19:return new F(function(){return _3f(_5n,_5i,_5p[2],_5p[3]);});break;case 20:return new F(function(){return _3f(_5n,_5h,_5p[2],_5p[3]);});break;case 21:return new F(function(){return _2x(_5n,_5g,_5p[2]);});break;case 22:return new F(function(){return _3S(_5n,_5b,_5p[2],_5p[3]);});break;case 23:return new F(function(){return _3s(_5n,_50,_5p[2],_5p[3]);});break;default:return new F(function(){return _2I(_5n,_4Z,_5p[2]);});}},_5x=false,_5y=true,_5z=function(_5A,_5B){while(1){var _5C=E(_5A);if(!_5C[0]){return E(_5B)[0]==0?true:false;}else{var _5D=E(_5B);if(!_5D[0]){return false;}else{if(E(_5C[1])[1]!=E(_5D[1])[1]){return false;}else{_5A=_5C[2];_5B=_5D[2];continue;}}}}},_5E=function(_5F,_5G){return !B(_5z(_5F,_5G))?true:false;},_5H=[0,_5z,_5E],_5I=[1],_5J=5,_5K=4,_5L=0,_5M=[0],_5N=[3,_5I,_5M],_5O=[2,_5N],_5P=[3,_5O,_5M],_5Q=[2,_5P],_5R=function(_5S){return [1,E(_5S)[1]];},_5T=function(_5U,_5V){while(1){var _5W=E(_5U);switch(_5W[0]){case 0:return E(_5V)[0]==0?true:false;case 1:return E(_5V)[0]==1?true:false;case 2:var _5X=E(_5V);if(_5X[0]==2){_5U=_5W[1];_5V=_5X[1];continue;}else{return false;}break;default:var _5Y=E(_5V);if(_5Y[0]==3){if(!B(_5T(_5W[1],_5Y[1]))){return false;}else{_5U=_5W[2];_5V=_5Y[2];continue;}}else{return false;}}}},_5Z=function(_60,_61){var _62=E(_61)[1];return _62<E(_60)[1]?[1,_62]:[1,_62+1|0];},_63=[0,0],_64=function(_65,_66,_67){var _68=function(_69,_6a){var _6b=E(_6a);switch(_6b[0]){case 0:return new F(function(){return A(_65,[_69,_6b[1]]);});break;case 1:return new F(function(){return A(_66,[_69,[0,_6b[1]]]);});break;case 2:return [2,new T(function(){return B(_68(_69,_6b[1]));}),new T(function(){return B(_68(_69,_6b[2]));})];case 3:return E(_6b);case 4:return [4,E(_6b[1]),new T(function(){return B(_68(_69,_6b[2]));})];case 5:return [5,E(_6b[1]),new T(function(){return B(_68(_69,_6b[2]));}),new T(function(){return B(_68(_69,_6b[3]));})];default:return [6,E(_6b[1]),_6b[2],new T(function(){return B(_68(new T(function(){return [0,E(_69)[1]+1|0];}),_6b[3]));})];}};return new F(function(){return _68(_63,_67);});},_6c=function(_6d,_6e){return new F(function(){return _64(function(_6f,_6g){var _6h=E(_6d),_6i=E(_6g);return !B(_5z(_6h[1],_6i[1]))?[0,_6i]:!B(_5T(_6h[2],_6i[2]))?[0,_6i]:B(_5R(_6f));},_5Z,_6e);});},_6j=function(_6k){return E(E(_6k)[1]);},_6l=function(_6m,_6n,_6o){while(1){var _6p=E(_6o);if(!_6p[0]){return false;}else{if(!B(A(_6j,[_6m,_6n,_6p[1]]))){_6o=_6p[2];continue;}else{return true;}}}},_6q=function(_6r,_6s){while(1){var _6t=E(_6r);if(!_6t[0]){return E(_6s)[0]==0?true:false;}else{var _6u=E(_6s);if(!_6u[0]){return false;}else{if(E(_6t[1])[1]!=E(_6u[1])[1]){return false;}else{_6r=_6t[2];_6s=_6u[2];continue;}}}}},_6v=function(_6w){return E(E(_6w)[1]);},_6x=[3,_5I,_5N],_6y=[0,121],_6z=[1,_6y,_b],_6A=[0,_6z,_5I],_6B=[0,_6A],_6C=[0,120],_6D=[1,_6C,_b],_6E=[0,_6D,_5I],_6F=[0,_6E],_6G=new T(function(){return B(unCStr("in*"));}),_6H=[3,_5O,_5N],_6I=[3,_5I,_6H],_6J=[0,_6G,_6I],_6K=[3,_6J],_6L=[4,E(_5K),_6K],_6M=[4,E(_5J),_6L],_6N=[2,_6M,_6B],_6O=[0,80],_6P=[1,_6O,_b],_6Q=[0,_6P,_5O],_6R=[0,_6Q],_6S=[2,_6N,_6R],_6T=[2,_6S,_6F],_6U=new T(function(){return B(_6c(_6A,_6T));}),_6V=[6,E(_5L),_5I,_6U],_6W=[4,E(_5K),_6V],_6X=[0,112],_6Y=[1,_6X,_b],_6Z=[0,_6Y,_5Q],_70=[0,_6Z],_71=[4,E(_5J),_70],_72=[2,_71,_6W],_73=new T(function(){return B(_6c(_6E,_72));}),_74=[6,E(_5L),_5I,_73],_75=new T(function(){return B(_6c(_6Q,_74));}),_76=[6,E(_5L),_5O,_75],_77=new T(function(){return B(_6c(_6Z,_76));}),_78=[6,E(_5L),_5Q,_77],_79=new T(function(){return B(unCStr("seek"));}),_7a=new T(function(){return B(unCStr("in"));}),_7b=[0,42],_7c=[1,_7b,_b],_7d=new T(function(){return B(unCStr("love"));}),_7e=[1,_7d,_b],_7f=new T(function(){return B(unCStr("eat"));}),_7g=[1,_7f,_7e],_7h=new T(function(){return B(unCStr("lose"));}),_7i=[1,_7h,_7g],_7j=new T(function(){return B(unCStr("find"));}),_7k=[1,_7j,_7i],_7l=[3,_5I,_5M],_7m=function(_7n){var _7o=E(_7n);switch(_7o[0]){case 0:return [0];case 1:return E(_7l);case 2:return E(_7l);case 3:return E(_7l);case 4:return [3,[2,new T(function(){return B(_7m(_7o[3]));})],new T(function(){return B(_7m(_7o[2]));})];default:return [3,[2,new T(function(){return B(_7m(_7o[3]));})],new T(function(){return B(_7m(_7o[2]));})];}},_7p=[1,_],_7q=[5,_,_7p,_7p],_7r=new T(function(){return B(_7m(_7q));}),_7s=new T(function(){return B(unCStr("try"));}),_7t=[0,_7s,_7r],_7u=[3,_7t],_7v=[0,_6P,_5Q],_7w=[0,_7v],_7x=[0,_],_7y=[4,_,_7x,_7p],_7z=[4,_,_7p,_7y],_7A=new T(function(){return B(_7m(_7z));}),_7B=[0,_7j,_7A],_7C=[3,_7B],_7D=new T(function(){return B(_7E(_7C));}),_7F=new T(function(){return [2,_7D,_7w];}),_7G=new T(function(){return [4,E(_5K),_7F];}),_7H=new T(function(){return [2,_7u,_7G];}),_7I=new T(function(){return B(_6c(_7v,_7H));}),_7J=new T(function(){return [6,E(_5L),_5Q,_7I];}),_7E=function(_7K){var _7L=E(_7K);switch(_7L[0]){case 2:return [2,new T(function(){return B(_7E(_7L[1]));}),new T(function(){return B(_7E(_7L[2]));})];case 3:var _7M=_7L[1];if(!B(_6l(_5H,new T(function(){return B(_6v(_7M));}),_7k))){var _7N=E(_7M)[1];return !B(_6q(_7N,_7a))?!B(_6q(_7N,_79))?E(_7L):E(_7J):E(_78);}else{return [6,E(_5L),_5Q,new T(function(){return B(_6c(_6Z,[6,E(_5L),_5I,new T(function(){return B(_6c(_6E,[2,_71,[4,E(_5K),[6,E(_5L),_5I,new T(function(){return B(_6c(_6A,[2,[2,[4,E(_5J),[4,E(_5K),[3,[0,new T(function(){return B(_N(E(_7M)[1],_7c));}),_6x]]]],_6B],_6F]));})]]]));})]));})];}break;case 4:return [4,E(_7L[1]),new T(function(){return B(_7E(_7L[2]));})];case 5:return [5,E(_7L[1]),new T(function(){return B(_7E(_7L[2]));}),new T(function(){return B(_7E(_7L[3]));})];case 6:return [6,E(_7L[1]),_7L[2],new T(function(){return B(_7E(_7L[3]));})];default:return E(_7L);}},_7O=new T(function(){return B(unCStr("Parsed:"));}),_7P=new T(function(){return B(unCStr("Translation:"));}),_7Q=new T(function(){return B(unCStr("Translation (simplified):"));}),_7R=new T(function(){return B(unCStr("Translation (MP applied):"));}),_7S=function(_7T,_7U){return E(_7T)[1]==E(_7U)[1];},_7V=function(_7W,_7X){return E(_7W)[1]!=E(_7X)[1];},_7Y=[0,_7S,_7V],_7Z=function(_80,_81){return [0,_81];},_82=function(_83,_84){var _85=E(_83);if(!_85){return E(_84);}else{return new F(function(){return _64(_7Z,function(_86,_87){var _88=E(_87)[1];return _88<E(_86)[1]?[1,_88]:[1,_88+_85|0];},_84);});}},_89=function(_8a){while(1){var _8b=(function(_8c){var _8d=E(_8c);if(!_8d[0]){return [0];}else{var _8e=_8d[2],_8f=E(E(_8d[1])[1]);if(!_8f){_8a=_8e;return null;}else{return [1,[0,_8f-1|0],new T(function(){return B(_89(_8e));})];}}})(_8a);if(_8b!=null){return _8b;}}},_8g=function(_8h){while(1){var _8i=(function(_8j){var _8k=E(_8j);switch(_8k[0]){case 1:return [1,[0,_8k[1]],_b];case 2:return new F(function(){return _N(B(_8g(_8k[1])),new T(function(){return B(_8g(_8k[2]));},1));});break;case 4:_8h=_8k[2];return null;case 5:return new F(function(){return _N(B(_8g(_8k[2])),new T(function(){return B(_8g(_8k[3]));},1));});break;case 6:return new F(function(){return _89(B(_8g(_8k[3])));});break;default:return [0];}})(_8h);if(_8i!=null){return _8i;}}},_8l=function(_8m,_8n){return new F(function(){return _64(_7Z,function(_8o,_8p){var _8q=E(_8p)[1],_8r=E(_8o)[1];if(_8q!=_8r){return _8q<=_8r?[1,_8q]:[1,_8q-1|0];}else{return new F(function(){return _82(_8r,_8m);});}},_8n);});},_8s=function(_8t){while(1){var _8u=(function(_8v){var _8w=E(_8v);switch(_8w[0]){case 2:var _8x=_8w[2],_8y=B(_8s(_8w[1]));if(_8y[0]==6){if(!E(_8y[1])){_8t=B(_8l(new T(function(){return B(_8s(_8x));}),_8y[3]));return null;}else{return [2,_8y,new T(function(){return B(_8s(_8x));})];}}else{return [2,_8y,new T(function(){return B(_8s(_8x));})];}break;case 4:var _8z=_8w[2],_8A=E(_8w[1]);if(_8A==5){var _8B=B(_8s(_8z));return _8B[0]==4?E(_8B[1])==4?E(_8B[2]):[4,E(_5J),_8B]:[4,E(_5J),_8B];}else{return [4,E(_8A),new T(function(){return B(_8s(_8z));})];}break;case 5:return [5,E(_8w[1]),new T(function(){return B(_8s(_8w[2]));}),new T(function(){return B(_8s(_8w[3]));})];case 6:var _8C=_8w[2],_8D=_8w[3],_8E=E(_8w[1]);if(!_8E){var _8F=B(_8s(_8D));if(_8F[0]==2){var _8G=_8F[1],_8H=E(_8F[2]);return _8H[0]==1?E(_8H[1])==0?!B(_6l(_7Y,_63,B(_8g(_8G))))?B(_82(-1,_8G)):[6,E(_5L),_8C,_8F]:[6,E(_5L),_8C,_8F]:[6,E(_5L),_8C,_8F];}else{return [6,E(_5L),_8C,_8F];}}else{return [6,E(_8E),_8C,new T(function(){return B(_8s(_8D));})];}break;default:return E(_8w);}})(_8t);if(_8u!=null){return _8u;}}},_8I=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_8J=new T(function(){return B(err(_8I));}),_8K=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_8L=new T(function(){return B(err(_8K));}),_8M=function(_8N,_8O){while(1){var _8P=E(_8N);if(!_8P[0]){return E(_8L);}else{var _8Q=E(_8O);if(!_8Q){return E(_8P[1]);}else{_8N=_8P[2];_8O=_8Q-1|0;continue;}}}},_8R=function(_8S,_8T){var _8U=E(_8S);if(!_8U[0]){return E(_8T);}else{return new F(function(){return A(_8U[1],[new T(function(){return B(_8R(_8U[2],_8T));})]);});}},_8V=[0,11],_8W=function(_8X,_8Y){return [0,_b,_8Y,_j];},_8Z=[0,120],_90=[0,10],_91=new T(function(){return B(unCStr(": empty list"));}),_92=new T(function(){return B(unCStr("Prelude."));}),_93=function(_94){return new F(function(){return err(B(_N(_92,new T(function(){return B(_N(_94,_91));},1))));});},_95=new T(function(){return B(unCStr("head"));}),_96=new T(function(){return B(_93(_95));}),_97=function(_98,_99){var _9a=E(_99);if(!_9a[0]){return [0];}else{var _9b=_9a[1],_9c=new T(function(){var _9d=B(_1k(new T(function(){return B(A(_98,[_9b]));}),_9a[2]));return [0,_9d[1],_9d[2]];});return [1,[1,_9b,new T(function(){return E(E(_9c)[1]);})],new T(function(){return B(_97(_98,E(_9c)[2]));})];}},_9e=function(_9f){var _9g=E(_9f);if(!_9g[0]){return [0];}else{return new F(function(){return _N(_9g[1],new T(function(){return B(_9e(_9g[2]));},1));});}},_9h=function(_9i){return E(_9i);},_9j=[0,32],_9k=new T(function(){return B(unCStr(" {"));}),_9l=[0,172],_9m=[1,_9l,_b],_9n=new T(function(){return B(unCStr("[]"));}),_9o=[0,9723],_9p=[1,_9o,_b],_9q=new T(function(){return B(unCStr("F "));}),_9r=new T(function(){return B(unCStr("H "));}),_9s=new T(function(){return B(unCStr("Int "));}),_9t=[0,708],_9u=[1,_9t,_b],_9v=new T(function(){return B(unCStr("Ext "));}),_9w=[0,125],_9x=[0,709],_9y=[1,_9x,_b],_9z=new T(function(){return B(unCStr("&&"));}),_9A=[0,8743],_9B=[1,_9A,_b],_9C=[0,4],_9D=new T(function(){return B(unCStr("||"));}),_9E=[0,8744],_9F=[1,_9E,_b],_9G=[0,3],_9H=new T(function(){return B(unCStr(", "));}),_9I=[0,8594],_9J=[1,_9I,_b],_9K=function(_9L){return [0,E(_9L)[1]+1|0];},_9M=new T(function(){return B(unCStr("<->"));}),_9N=[0,8596],_9O=[1,_9N,_b],_9P=[0,61],_9Q=[1,_9P,_b],_9R=[0,5],_9S=[0,_9Q,_9R,_9K,_9K],_9T=function(_9U){return new F(function(){return _N(_9H,_9U);});},_9V=[0,92],_9W=[1,_9V,_b],_9X=[0,955],_9Y=[1,_9X,_b],_9Z=new T(function(){return B(unCStr("forall "));}),_a0=[0,8704],_a1=[1,_a0,_b],_a2=new T(function(){return B(unCStr("exists "));}),_a3=[0,8707],_a4=[1,_a3,_b],_a5=function(_a6){return E(E(_a6)[2]);},_a7=function(_a8,_a9){return new F(function(){return _5T(B(_a5(_a8)),B(_a5(_a9)));});},_aa=[0,40],_ab=new T(function(){return B(unCStr(". "));}),_ac=[0,41],_ad=[0,123],_ae=new T(function(){return B(unCStr("not "));}),_af=function(_ag,_ah){var _ai=E(_ah);return _ai[0]==0?[0]:[1,new T(function(){return B(A(_ag,[_ai[1]]));}),new T(function(){return B(_af(_ag,_ai[2]));})];},_aj=function(_ak,_al){var _am=E(_al);return _am[0]==0?[0]:[1,_ak,[1,_am[1],new T(function(){return B(_aj(_ak,_am[2]));})]];},_an=[0,1],_ao=[0,115],_ap=new T(function(){return B(unCStr("->"));}),_aq=[0,101],_ar=[1,_aq,_b],_as=function(_9U){return new F(function(){return _N(_ar,_9U);});},_at=[0,116],_au=[1,_at,_b],_av=function(_9U){return new F(function(){return _N(_au,_9U);});},_aw=function(_ax,_ay,_az){var _aA=function(_aB,_aC){var _aD=E(_aC);switch(_aD[0]){case 0:return E(_av);case 1:return E(_as);case 2:var _aE=new T(function(){return B(_aA(_63,_aD[1]));}),_aF=function(_aG){return !E(_ax)?B(_N(_ap,new T(function(){return B(A(_aE,[_aG]));},1))):[1,_9I,new T(function(){return B(A(_aE,[_aG]));})];};return E(_aB)[1]<=0?function(_aH){return [1,_ao,new T(function(){return B(_aF(_aH));})];}:function(_aI){return [1,_22,[1,_ao,new T(function(){return B(_aF([1,_21,_aI]));})]];};default:var _aJ=new T(function(){return B(_aA(_63,_aD[2]));}),_aK=function(_aL){return !E(_ax)?B(_N(_ap,new T(function(){return B(A(_aJ,[_aL]));},1))):[1,_9I,new T(function(){return B(A(_aJ,[_aL]));})];},_aM=new T(function(){return B(_aA(_an,_aD[1]));});return E(_aB)[1]<=0?function(_aN){return new F(function(){return A(_aM,[new T(function(){return B(_aK(_aN));})]);});}:function(_aO){return [1,_22,new T(function(){return B(A(_aM,[new T(function(){return B(_aK([1,_21,_aO]));})]));})];};}};return new F(function(){return _aA(_ay,_az);});},_aP=function(_aQ,_aR){while(1){var _aS=E(_aQ);if(!_aS[0]){return E(_aR);}else{_aQ=_aS[2];var _aT=[1,_aS[1],_aR];_aR=_aT;continue;}}},_aU=function(_aV,_aW,_aX,_aY){var _aZ=new T(function(){return E(_aX)[1]>10;}),_b0=function(_b1){var _b2=E(_b1);if(!_b2[0]){return [0];}else{var _b3=_b2[1];return [1,new T(function(){var _b4=new T(function(){return B(unAppCStr(" : ",new T(function(){var _b5=E(_b3);if(!_b5[0]){var _b6=E(_96);}else{var _b6=B(A(_aw,[_aV,_63,E(_b5[1])[2],_b]));}return _b6;})));},1),_b7=B(_af(_6v,_b3));if(!_b7[0]){var _b8=E(_b4);}else{var _b8=B(_N(B(_9e([1,_b7[1],new T(function(){return B(_aj(_9H,_b7[2]));})])),_b4));}return _b8;}),new T(function(){return B(_b0(_b2[2]));})];}},_b9=function(_ba,_bb){var _bc=E(_bb);switch(_bc[0]){case 0:return function(_bd,_be){return [0,function(_9U){return new F(function(){return _N(new T(function(){return E(E(_bc[1])[1]);}),_9U);});},_be,_j];};case 1:var _bf=_bc[1];return function(_bg,_bh){return [0,function(_9U){return new F(function(){return _N(new T(function(){if(!E(new T(function(){return _bf<0;}))){var _bi=E(B(_8M(_bg,_bf))[1]);}else{var _bi=E(_8J);}return _bi;}),_9U);});},_bh,_j];};case 2:var _bj=_bc[1],_bk=_bc[2];if(!E(_aW)){var _bl=function(_bm){return function(_bn,_bo){var _bp=new T(function(){return B(A(new T(function(){return B(_b9(_90,_bj));}),[_bn,_bo]));}),_bq=new T(function(){return B(A(new T(function(){return B(_b9(_8V,_bk));}),[_bn,new T(function(){return E(E(_bp)[2]);})]));});return [0,new T(function(){if(!E(new T(function(){return E(_ba)[1]>10;}))){var _br=function(_bs){return new F(function(){return A(E(_bp)[1],[[1,_9j,new T(function(){return B(A(E(_bq)[1],[_bs]));})]]);});};}else{var _br=function(_bt){return [1,_22,new T(function(){return B(A(E(_bp)[1],[[1,_9j,new T(function(){return B(A(E(_bq)[1],[[1,_21,_bt]]));})]]));})];};}return _br;}),new T(function(){return E(E(_bq)[2]);}),_j];};},_bu=E(_bj);if(_bu[0]==4){if(E(_bu[1])==5){return function(_bv,_bw){var _bx=new T(function(){return B(A(new T(function(){return B(_b9(_8V,_bu[2]));}),[_bv,_bw]));}),_by=new T(function(){return B(A(new T(function(){return B(_b9(_63,_bk));}),[_bv,new T(function(){return E(E(_bx)[2]);})]));});return [0,new T(function(){if(!E(new T(function(){return E(_ba)[1]>10;}))){var _bz=function(_bA){return new F(function(){return A(E(_bx)[1],[new T(function(){return B(_N(_9k,new T(function(){return B(A(E(_by)[1],[[1,_9w,_bA]]));},1)));})]);});};}else{var _bz=function(_bB){return [1,_22,new T(function(){return B(A(E(_bx)[1],[new T(function(){return B(_N(_9k,new T(function(){return B(A(E(_by)[1],[[1,_9w,[1,_21,_bB]]]));},1)));})]));})];};}return _bz;}),new T(function(){return E(E(_by)[2]);}),_j];};}else{return new F(function(){return _bl(_);});}}else{return new F(function(){return _bl(_);});}}else{return new F(function(){return _bC(_bj,[1,_bk,_b]);});}break;case 3:return function(_bD,_bE){return [0,function(_9U){return new F(function(){return _N(new T(function(){return E(E(_bc[1])[1]);}),_9U);});},_bE,_j];};case 4:var _bF=_bc[1],_bG=new T(function(){switch(E(_bF)){case 4:var _bH=!E(_aV)?E(_90):E(_8V);break;case 5:var _bH=!E(_aV)?E(_90):E(_8V);break;default:var _bH=E(_90);}return _bH;}),_bI=new T(function(){switch(E(_bF)){case 0:var _bJ=!E(_aV)?E(_ae):E(_9m);break;case 1:var _bJ=!E(_aV)?E(_9n):E(_9p);break;case 2:var _bJ=E(_9q);break;case 3:var _bJ=E(_9r);break;case 4:var _bJ=!E(_aV)?E(_9s):E(_9u);break;default:var _bJ=!E(_aV)?E(_9v):E(_9y);}return _bJ;});return function(_bK,_bL){var _bM=new T(function(){return B(A(new T(function(){return B(_b9(new T(function(){return [0,E(_bG)[1]+1|0];},1),_bc[2]));}),[_bK,_bL]));});return [0,new T(function(){if(!E(new T(function(){return E(_ba)[1]>E(_bG)[1];}))){var _bN=function(_bO){return new F(function(){return _N(_bI,new T(function(){return B(A(E(_bM)[1],[_bO]));},1));});};}else{var _bN=function(_bP){return [1,_22,new T(function(){return B(_N(_bI,new T(function(){return B(A(E(_bM)[1],[[1,_21,_bP]]));},1)));})];};}return _bN;}),new T(function(){return E(E(_bM)[2]);}),_j];};case 5:var _bQ=new T(function(){switch(E(_bc[1])){case 0:var _bR=E([0,new T(function(){return !E(_aV)?E(_9z):E(_9B);}),_9C,_9h,_9h]);break;case 1:var _bR=E([0,new T(function(){return !E(_aV)?E(_9D):E(_9F);}),_9G,_9h,_9h]);break;case 2:var _bR=E([0,new T(function(){return !E(_aV)?E(_ap):E(_9J);}),_an,_9K,_9h]);break;case 3:var _bR=E([0,new T(function(){return !E(_aV)?E(_9M):E(_9O);}),_an,_9K,_9K]);break;default:var _bR=E(_9S);}return _bR;}),_bS=new T(function(){return E(E(_bQ)[2]);}),_bT=new T(function(){return E(E(_bQ)[1]);});return function(_bU,_bV){var _bW=new T(function(){return B(A(new T(function(){return B(_b9(new T(function(){return B(A(E(_bQ)[3],[_bS]));},1),_bc[2]));}),[_bU,_bV]));}),_bX=new T(function(){return B(A(new T(function(){return B(_b9(new T(function(){return B(A(E(_bQ)[4],[_bS]));},1),_bc[3]));}),[_bU,new T(function(){return E(E(_bW)[2]);})]));});return [0,new T(function(){if(!E(new T(function(){return E(_ba)[1]>E(_bS)[1];}))){var _bY=function(_bZ){return new F(function(){return A(E(_bW)[1],[[1,_9j,new T(function(){return B(_N(_bT,[1,_9j,new T(function(){return B(A(E(_bX)[1],[_bZ]));})]));})]]);});};}else{var _bY=function(_c0){return [1,_22,new T(function(){return B(A(E(_bW)[1],[[1,_9j,new T(function(){return B(_N(_bT,[1,_9j,new T(function(){return B(A(E(_bX)[1],[[1,_21,_c0]]));})]));})]]));})];};}return _bY;}),new T(function(){return E(E(_bX)[2]);}),_j];};default:var _c1=_bc[1];return function(_c2,_c3){var _c4=new T(function(){var _c5=B(A(new T(function(){var _c6=function(_c7){var _c8=E(_c7);if(_c8[0]==6){var _c9=_c8[1],_ca=function(_cb){return function(_cc,_cd){var _ce=new T(function(){var _cf=[0,[1,_8Z,new T(function(){return B(_2R(0,E(_cd)[1],_b));})],_c8[2]],_cg=B(A(new T(function(){return B(_c6(_c8[3]));}),[[1,_cf,_cc],new T(function(){return [0,E(_cd)[1]+1|0];})])),_ch=E(_cg[1]);return [0,[0,[1,_cf,_ch[1]],_ch[2]],_cg[2],_j];});return [0,new T(function(){return E(E(_ce)[1]);}),new T(function(){return E(E(_ce)[2]);}),_j];};};switch(E(_c1)){case 0:if(!E(_c9)){return new F(function(){return _ca(_);});}else{return function(_ci,_cj){var _ck=new T(function(){return B(A(new T(function(){return B(_b9(_63,_c8));}),[_ci,_cj]));});return [0,[0,_b,new T(function(){return E(E(_ck)[1]);})],new T(function(){return E(E(_ck)[2]);}),_j];};}break;case 1:if(E(_c9)==1){return new F(function(){return _ca(_);});}else{return function(_cl,_cm){var _cn=new T(function(){return B(A(new T(function(){return B(_b9(_63,_c8));}),[_cl,_cm]));});return [0,[0,_b,new T(function(){return E(E(_cn)[1]);})],new T(function(){return E(E(_cn)[2]);}),_j];};}break;default:if(E(_c9)==2){return new F(function(){return _ca(_);});}else{return function(_co,_cp){var _cq=new T(function(){return B(A(new T(function(){return B(_b9(_63,_c8));}),[_co,_cp]));});return [0,[0,_b,new T(function(){return E(E(_cq)[1]);})],new T(function(){return E(E(_cq)[2]);}),_j];};}}}else{return function(_cr,_cs){var _ct=new T(function(){return B(A(new T(function(){return B(_b9(_63,_c8));}),[_cr,_cs]));});return [0,[0,_b,new T(function(){return E(E(_ct)[1]);})],new T(function(){return E(E(_ct)[2]);}),_j];};}};return B(_c6(_bc));}),[_c2,_c3])),_cu=E(_c5[1]),_cv=function(_cw){return new F(function(){return _N(new T(function(){switch(E(_c1)){case 0:var _cx=!E(_aV)?E(_9W):E(_9Y);break;case 1:var _cx=!E(_aV)?E(_9Z):E(_a1);break;default:var _cx=!E(_aV)?E(_a2):E(_a4);}return _cx;}),new T(function(){return B(_N(new T(function(){var _cy=B(_b0(B(_97(_a7,_cu[1]))));return _cy[0]==0?[0]:B(_9e([1,_cy[1],new T(function(){return B(_aj(_9H,_cy[2]));})]));}),new T(function(){return B(_N(_ab,new T(function(){return B(A(_cu[2],[_cw]));},1)));},1)));},1));});};return [0,new T(function(){if(!E(new T(function(){return E(_ba)[1]>0;}))){var _cz=E(_cv);}else{var _cz=function(_cA){return [1,_22,new T(function(){return B(_cv([1,_21,_cA]));})];};}return _cz;}),_c5[2],_j];});return [0,new T(function(){return E(E(_c4)[1]);}),new T(function(){return E(E(_c4)[2]);}),_j];};}},_bC=function(_cB,_cC){while(1){var _cD=(function(_cE,_cF){var _cG=E(_cE);if(_cG[0]==2){_cB=_cG[1];var _cH=[1,_cG[2],_cF];_cC=_cH;return null;}else{return function(_cI,_cJ){var _cK=new T(function(){var _cL=new T(function(){return B(A(new T(function(){return B(_cM(B(_aP(_cF,_b))));}),[_cI,_cJ]));});return B(A(function(_cN){var _cO=new T(function(){var _cP=E(_cN);return _cP[0]==0?[0]:[1,_cP[1],new T(function(){return B(_aj(_9T,_cP[2]));})];}),_cQ=function(_cR,_cS){var _cT=new T(function(){return B(A(new T(function(){return B(_b9(_8V,_cG));}),[_cR,_cS]));});return [0,new T(function(){if(!E(_aZ)){var _cU=function(_cV){return new F(function(){return A(E(_cT)[1],[[1,_aa,new T(function(){return B(_8R(_cO,[1,_ac,_cV]));})]]);});};}else{var _cU=function(_cW){return [1,_22,new T(function(){return B(A(E(_cT)[1],[[1,_aa,new T(function(){return B(_8R(_cO,[1,_ac,[1,_21,_cW]]));})]]));})];};}return _cU;}),new T(function(){return E(E(_cT)[2]);}),_j];},_cX=E(_cG);return _cX[0]==4?E(_cX[1])==5?function(_cY,_cZ){var _d0=new T(function(){return B(A(new T(function(){return B(_b9(_8V,_cX[2]));}),[_cY,_cZ]));});return [0,new T(function(){if(!E(_aZ)){var _d1=function(_d2){return new F(function(){return A(E(_d0)[1],[[1,_ad,new T(function(){return B(_8R(_cO,[1,_9w,_d2]));})]]);});};}else{var _d1=function(_d3){return [1,_22,new T(function(){return B(A(E(_d0)[1],[[1,_ad,new T(function(){return B(_8R(_cO,[1,_9w,[1,_21,_d3]]));})]]));})];};}return _d1;}),new T(function(){return E(E(_d0)[2]);}),_j];}:function(_d4,_d5){var _d6=B(_cQ(_d4,_d5));return [0,_d6[1],_d6[2],_d6[3]];}:function(_d7,_d8){var _d9=B(_cQ(_d7,_d8));return [0,_d9[1],_d9[2],_d9[3]];};},[new T(function(){return E(E(_cL)[1]);},1),_cI,new T(function(){return E(E(_cL)[2]);})]));});return [0,new T(function(){return E(E(_cK)[1]);}),new T(function(){return E(E(_cK)[2]);}),_j];};}})(_cB,_cC);if(_cD!=null){return _cD;}}},_cM=function(_da){var _db=E(_da);return _db[0]==0?E(_8W):function(_dc,_dd){var _de=new T(function(){return B(A(new T(function(){return B(_b9(_63,_db[1]));}),[_dc,_dd]));}),_df=new T(function(){return B(A(new T(function(){return B(_cM(_db[2]));}),[_dc,new T(function(){return E(E(_de)[2]);})]));});return [0,[1,new T(function(){return E(E(_de)[1]);}),new T(function(){return E(E(_df)[1]);})],new T(function(){return E(E(_df)[2]);}),_j];};};return E(B(A(_b9,[_aX,_aY,_b,_63]))[1]);},_dg=0,_dh=2,_di=2,_dj=3,_dk=0,_dl=1,_dm=[4,_,_7x,_7p],_dn=new T(function(){return B(_7m(_dm));}),_do=[2,_dn],_dp=[2,_7l],_dq=new T(function(){return B(_7m(_7p));}),_dr=[2,_dq],_ds=[0,121],_dt=[1,_ds,_b],_du=[0,_dt,_5I],_dv=[0,_du],_dw=[0,80],_dx=[1,_dw,_b],_dy=[0,_dx,_dp],_dz=[0,_dy],_dA=[4,E(_5J),_dz],_dB=[2,_dA,_dv],_dC=new T(function(){return B(_6c(_dy,_dB));}),_dD=[6,E(_5L),_dp,_dC],_dE=[4,E(_5K),_dD],_dF=new T(function(){return B(_1F("../src/Translation.hs:(90,3)-(93,85)|case"));}),_dG=function(_dH){return new F(function(){return _1F("../src/Translation.hs:(82,3)-(87,37)|case");});},_dI=new T(function(){return B(_dG(_));}),_dJ=new T(function(){return B(_1F("../src/Translation.hs:(76,3)-(78,80)|case"));}),_dK=[4,_,_7p,_dm],_dL=new T(function(){return B(_7m(_dK));}),_dM=[2,_dL],_dN=[0,_dx,_do],_dO=[0,_dN],_dP=[4,E(_5J),_dO],_dQ=[0,82],_dR=[1,_dQ,_b],_dS=[0,_dR,_dM],_dT=[0,_dS],_dU=[4,E(_5J),_dT],_dV=[0,81],_dW=[1,_dV,_b],_dX=[0,_dW,_dr],_dY=[0,_dX],_dZ=[4,E(_5J),_dY],_e0=[0,120],_e1=[1,_e0,_b],_e2=[0,_e1,_5I],_e3=[0,_e2],_e4=[2,_dZ,_e3],_e5=new T(function(){return B(_6c(_dX,_e4));}),_e6=[6,E(_5L),_dr,_e5],_e7=[4,E(_5K),_e6],_e8=[2,_dU,_e7],_e9=[2,_e8,_dv],_ea=new T(function(){return B(_6c(_du,_e9));}),_eb=[6,E(_5L),_5I,_ea],_ec=[4,E(_5K),_eb],_ed=[2,_dP,_ec],_ee=new T(function(){return B(_6c(_e2,_ed));}),_ef=[6,E(_5L),_5I,_ee],_eg=new T(function(){return B(_6c(_dS,_ef));}),_eh=[6,E(_5L),_dM,_eg],_ei=new T(function(){return B(_6c(_dN,_eh));}),_ej=[6,E(_5L),_do,_ei],_ek=new T(function(){return B(unCStr("by"));}),_el=[2,_],_em=[4,_,_dm,_el],_en=new T(function(){return B(_7m(_em));}),_eo=new T(function(){return B(unCStr("the"));}),_ep=new T(function(){return B(unCStr("no"));}),_eq=new T(function(){return B(unCStr("every"));}),_er=[0,112],_es=[1,_er,_b],_et=[0,_es,_dp],_eu=[0,_et],_ev=[4,E(_5J),_eu],_ew=1,_ex=[2,_ev,_e3],_ey=[0,113],_ez=[1,_ey,_b],_eA=[0,_ez,_dp],_eB=[0,_eA],_eC=[4,E(_5J),_eB],_eD=[2,_eC,_e3],_eE=[5,E(_dg),_ex,_eD],_eF=[4,E(_dk),_eE],_eG=new T(function(){return B(_6c(_e2,_eF));}),_eH=[6,E(_ew),_5I,_eG],_eI=new T(function(){return B(_6c(_eA,_eH));}),_eJ=[6,E(_5L),_dp,_eI],_eK=new T(function(){return B(_6c(_et,_eJ));}),_eL=[6,E(_5L),_dp,_eK],_eM=[0,_dx,_dr],_eN=[0,_eM],_eO=[4,E(_5J),_eN],_eP=[2,_eO,_e3],_eQ=new T(function(){return B(_6c(_eM,_eP));}),_eR=[6,E(_5L),_dr,_eQ],_eS=[4,E(_5K),_eR],_eT=2,_eU=[5,E(_eT),_ex,_eD],_eV=new T(function(){return B(_6c(_e2,_eU));}),_eW=[6,E(_ew),_5I,_eV],_eX=new T(function(){return B(_6c(_eA,_eW));}),_eY=[6,E(_5L),_dp,_eX],_eZ=new T(function(){return B(_6c(_et,_eY));}),_f0=[6,E(_5L),_dp,_eZ],_f1=new T(function(){return B(_6c(_e2,_eE));}),_f2=[6,E(_dh),_5I,_f1],_f3=new T(function(){return B(_6c(_eA,_f2));}),_f4=[6,E(_5L),_dp,_f3],_f5=new T(function(){return B(_6c(_et,_f4));}),_f6=[6,E(_5L),_dp,_f5],_f7=3,_f8=4,_f9=[5,E(_f8),_e3,_dv],_fa=[5,E(_f7),_ex,_f9],_fb=new T(function(){return B(_6c(_e2,_fa));}),_fc=[6,E(_ew),_5I,_fb],_fd=[2,_eC,_dv],_fe=[5,E(_dg),_fc,_fd],_ff=new T(function(){return B(_6c(_du,_fe));}),_fg=[6,E(_dh),_5I,_ff],_fh=new T(function(){return B(_6c(_eA,_fg));}),_fi=[6,E(_5L),_dp,_fh],_fj=new T(function(){return B(_6c(_et,_fi));}),_fk=[6,E(_5L),_dp,_fj],_fl=[3,_dp,_5M],_fm=[2,_fl],_fn=[0,_es,_fm],_fo=[0,_fn],_fp=[4,E(_5J),_fo],_fq=new T(function(){return B(_6c(_du,_f9));}),_fr=[6,E(_5L),_5I,_fq],_fs=[4,E(_5K),_fr],_ft=[2,_fp,_fs],_fu=new T(function(){return B(_6c(_e2,_ft));}),_fv=[6,E(_5L),_5I,_fu],_fw=new T(function(){return B(_6c(_fn,_fv));}),_fx=[6,E(_5L),_fm,_fw],_fy=new T(function(){return B(unCStr("be"));}),_fz=[0,_dW,_do],_fA=[0,_fz],_fB=[2,_dA,_e3],_fC=new T(function(){return B(_6c(_e2,_fB));}),_fD=[6,E(_5L),_5I,_fC],_fE=new T(function(){return B(_6c(_dy,_fD));}),_fF=[6,E(_5L),_dp,_fE],_fG=[2,_5M],_fH=1,_fI=[0,_es,_fG],_fJ=[0,_fI],_fK=[4,E(_5J),_fJ],_fL=[4,E(_fH),_fK],_fM=new T(function(){return B(_6c(_fI,_fL));}),_fN=[6,E(_5L),_fG,_fM],_fO=new T(function(){return B(unCStr("necessarily"));}),_fP=[0,_es,_dr],_fQ=[0,_fP],_fR=[4,E(_5J),_fQ],_fS=function(_fT,_fU){return [2,new T(function(){return B(_fV(_fT));}),[4,E(_5K),new T(function(){return B(_fV(_fU));})]];},_fW=function(_fX){return [0,new T(function(){return B(unAppCStr("he_",new T(function(){return B(_2R(0,E(_fX)[1],_b));})));}),_5I];},_fV=function(_fY){var _fZ=E(_fY);switch(_fZ[0]){case 0:var _g0=_fZ[2],_g1=E(_fZ[1]);if(_g1[0]==4){var _g2=_g1[3],_g3=E(_g1[2]);switch(_g3[0]){case 0:switch(E(_g2)[0]){case 0:return !B(_6q(_g0,_fO))?[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:E(_fN);case 1:return [6,E(_5L),_dr,new T(function(){return B(_6c(_fP,[2,_fR,[3,[0,_g0,_5I]]]));})];default:return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}break;case 1:var _g4=E(_g2);switch(_g4[0]){case 3:return !B(_6q(_g0,_fy))?[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:E(_fF);case 4:return E(_g4[2])[0]==0?E(_g4[3])[0]==1?!B(_6q(_g0,_fy))?[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:E(_fx):[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:[3,[0,_g0,new T(function(){return B(_7m(_g1));})]];default:return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}break;case 4:var _g5=_g3[3];switch(E(_g3[2])[0]){case 0:if(E(_g5)[0]==1){if(E(_g2)[0]==2){var _g6=function(_g7){return !B(_6q(_g0,_eq))?!B(_6q(_g0,_ep))?!B(_6q(_g0,_eo))?[3,[0,_g0,_en]]:E(_fk):E(_eL):E(_f0);},_g8=E(_g0);if(!_g8[0]){return new F(function(){return _g6(_);});}else{return E(E(_g8[1])[1])==97?E(_g8[2])[0]==0?E(_f6):B(_g6(_)):B(_g6(_));}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}break;case 1:var _g9=E(_g5);if(_g9[0]==4){if(E(_g9[2])[0]==1){var _ga=E(_g9[3]);if(_ga[0]==4){if(!E(_ga[2])[0]){if(E(_ga[3])[0]==1){var _gb=E(_g2);return _gb[0]==4?E(_gb[2])[0]==0?E(_gb[3])[0]==1?!B(_6q(_g0,_ek))?[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:E(_ej):[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:[3,[0,_g0,new T(function(){return B(_7m(_g1));})]]:[3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}break;default:return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}break;default:return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}}else{return [3,[0,_g0,new T(function(){return B(_7m(_g1));})]];}break;case 1:return [6,E(_5L),_dp,new T(function(){return B(_6c(_et,[2,_ev,[0,new T(function(){return B(_fW(_fZ[2]));})]]));})];case 2:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 3:var _gc=_fZ[2];return [6,E(_5L),_5I,new T(function(){return B(_6c([0,new T(function(){return B(unAppCStr("he_",new T(function(){return B(_2R(0,E(_gc)[1],_b));})));}),_5I],[5,E(_dg),[2,new T(function(){return B(_fV(_fZ[3]));}),[0,new T(function(){return B(_fW(_gc));})]],new T(function(){return B(_fV(_fZ[4]));})]));})];case 4:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 5:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 6:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 7:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 8:var _gd=_fZ[2],_ge=_fZ[3];switch(E(_fZ[1])[0]){case 0:return [5,E(_dg),new T(function(){return B(_fV(_gd));}),new T(function(){return B(_fV(_ge));})];case 1:return [6,E(_5L),_5I,new T(function(){return B(_6c(_e2,[5,E(_dg),[2,new T(function(){return B(_fV(_gd));}),_e3],[2,new T(function(){return B(_fV(_ge));}),_e3]]));})];default:return E(_dJ);}break;case 9:var _gf=_fZ[2],_gg=_fZ[3],_gh=E(_fZ[1]);switch(_gh[0]){case 0:return [5,E(_dl),new T(function(){return B(_fV(_gf));}),new T(function(){return B(_fV(_gg));})];case 1:return [6,E(_5L),_5I,new T(function(){return B(_6c(_e2,[5,E(_dl),[2,new T(function(){return B(_fV(_gf));}),_e3],[2,new T(function(){return B(_fV(_gg));}),_e3]]));})];case 4:return E(_gh[2])[0]==0?E(_gh[3])[0]==1?[6,E(_5L),_dp,new T(function(){return B(_6c(_dy,[5,E(_dl),[2,new T(function(){return B(_fV(_gf));}),_dz],[2,new T(function(){return B(_fV(_gg));}),_dz]]));})]:E(_dI):E(_dI);default:return E(_dI);}break;case 10:var _gi=_fZ[2],_gj=_fZ[3],_gk=_fZ[4];switch(E(_fZ[1])[0]){case 0:return [2,new T(function(){return B(_fV(_gj));}),[4,E(_5K),[6,E(_5L),_5I,new T(function(){return B(_6c([0,new T(function(){return B(unAppCStr("he_",new T(function(){return B(_2R(0,E(_gi)[1],_b));})));}),_5I],B(_fV(_gk))));})]]];case 1:return [6,E(_5L),_5I,new T(function(){return B(_6c(_du,[2,new T(function(){return B(_fV(_gj));}),[4,E(_5K),[6,E(_5L),_5I,new T(function(){return B(_6c([0,new T(function(){return B(unAppCStr("he_",new T(function(){return B(_2R(0,E(_gi)[1],_b));})));}),_5I],[2,new T(function(){return B(_fV(_gk));}),_dv]));})]]]));})];case 2:return [6,E(_5L),_5I,new T(function(){return B(_6c(_du,[2,new T(function(){return B(_fV(_gj));}),[4,E(_5K),[6,E(_5L),_5I,new T(function(){return B(_6c([0,new T(function(){return B(unAppCStr("he_",new T(function(){return B(_2R(0,E(_gi)[1],_b));})));}),_5I],[2,new T(function(){return B(_fV(_gk));}),_dv]));})]]]));})];default:return E(_dF);}break;case 11:return [4,E(_dk),new T(function(){return B(_fS(_fZ[2],_fZ[3]));})];case 12:return [4,E(_di),new T(function(){return B(_fS(_fZ[2],_fZ[3]));})];case 13:return [4,E(_dk),[4,E(_di),new T(function(){return B(_fS(_fZ[2],_fZ[3]));})]];case 14:return [4,E(_dj),new T(function(){return B(_fS(_fZ[2],_fZ[3]));})];case 15:return [4,E(_dk),[4,E(_dj),new T(function(){return B(_fS(_fZ[2],_fZ[3]));})]];case 16:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 17:return [2,new T(function(){return B(_fV(_fZ[2]));}),[4,E(_5K),new T(function(){return B(_fV(_fZ[3]));})]];case 18:return [6,E(_5L),_5I,new T(function(){return B(_6c(_e2,[6,E(_dh),_5I,new T(function(){return B(_6c(_du,[2,[2,new T(function(){return B(_fV(_fZ[2]));}),_dE],_e3]));})]));})];case 19:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 20:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 21:return [6,E(_5L),_do,new T(function(){return B(_6c(_dN,[6,E(_5L),_do,new T(function(){return B(_6c(_fz,[6,E(_5L),_5I,new T(function(){return B(_6c(_e2,[2,[2,[2,new T(function(){return B(_fV(_fZ[2]));}),_fA],_dO],_e3]));})]));})]));})];case 22:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;case 23:return new F(function(){return _fS(_fZ[2],_fZ[3]);});break;default:return [6,E(_5L),_5I,new T(function(){return B(_6c(_e2,[6,E(_dh),_5I,new T(function(){return B(_6c(_du,[4,E(_dj),[2,[2,new T(function(){return B(_fV(_fZ[2]));}),_eS],_dv]]));})]));})];}},_gl=function(_gm){var _gn=E(_gm),_go=_gn[1],_gp=_gn[2],_gq=new T(function(){return B(_fV(_gp));}),_gr=new T(function(){return B(_7m(_go));}),_gs=new T(function(){return B(_8s(_gq));});return [0,_7O,[1,new T(function(){return B(unAppCStr("  ",new T(function(){return B(A(_2i,[_2f,_gp,_b]));})));}),[1,new T(function(){return B(unAppCStr("  : ",new T(function(){return B(A(_23,[_2f,_go,_b]));})));}),[1,_b,[1,_7P,[1,new T(function(){return B(unAppCStr("  ",new T(function(){return B(A(_aU,[_5y,_5x,_2f,_gq,_b]));})));}),[1,new T(function(){return B(unAppCStr("  : ",new T(function(){return B(A(_aw,[_5y,_2f,_gr,_b]));})));}),[1,_b,[1,_7Q,[1,new T(function(){return B(unAppCStr("  ",new T(function(){return B(A(_aU,[_5y,_5x,_2f,_gs,_b]));})));}),[1,new T(function(){return B(unAppCStr("  : ",new T(function(){return B(A(_aw,[_5y,_2f,_gr,_b]));})));}),[1,_b,[1,_7R,[1,new T(function(){return B(unAppCStr("  ",new T(function(){return B(A(_aU,[_5y,_5x,_2f,B(_8s(B(_7E(_gs)))),_b]));})));}),[1,new T(function(){return B(unAppCStr("  : ",new T(function(){return B(A(_aw,[_5y,_2f,_gr,_b]));})));}),_b]]]]]]]]]]]]]]];},_gt=function(_gu){var _gv=B(_gl(_gu));return [1,_gv[1],_gv[2]];},_gw=function(_gx,_gy,_gz,_gA,_gB){var _gC=function(_gD){while(1){var _gE=(function(_gF){var _gG=E(_gF);if(!_gG[0]){return [0];}else{var _gH=_gG[2],_gI=E(_gG[1]),_gJ=B(A(_gy,[_gI[1],_gz,_gI[2],_gI[3]]));if(!_gJ[0]){_gD=_gH;return null;}else{return [1,_gJ[1],new T(function(){return B(_N(_gJ[2],new T(function(){return B(_gC(_gH));},1)));})];}}})(_gD);if(_gE!=null){return _gE;}}};return new F(function(){return _gC(B(A(_gx,[_gz,_gA,_gB])));});},_gK=[4,_,_7x,_7p],_gL=[4,_,_7p,_gK],_gM=[1],_gN=function(_gO,_gP,_gQ){var _gR=function(_gS){while(1){var _gT=(function(_gU){var _gV=E(_gU);if(!_gV[0]){return [0];}else{var _gW=_gV[2],_gX=E(_gV[1]),_gY=_gX[1],_gZ=function(_h0){var _h1=E(_h0);if(!_h1[0]){return [0];}else{var _h2=E(_h1[1]);return [1,[0,_gY,_h2[2],_h2[3]],new T(function(){return B(_N(_b,new T(function(){return B(_gZ(_h1[2]));},1)));})];}};if(!B(_6q(_gO,_gY))){var _h3=B(_gZ(_b));if(!_h3[0]){_gS=_gW;return null;}else{return [1,_h3[1],new T(function(){return B(_N(_h3[2],new T(function(){return B(_gR(_gW));},1)));})];}}else{var _h4=B(_gZ([1,[0,_j,_gX[2],_gX[3]],_b]));if(!_h4[0]){_gS=_gW;return null;}else{return [1,_h4[1],new T(function(){return B(_N(_h4[2],new T(function(){return B(_gR(_gW));},1)));})];}}}})(_gS);if(_gT!=null){return _gT;}}},_h5=E(_gQ);return _h5[0]==0?B(_gR(_b)):B(_gR([1,[0,_h5[1],_gP,_h5[2]],_b]));},_h6=new T(function(){return B(unCStr("to"));}),_h7=function(_h8,_h9,_ha){return [0];},_hb=function(_hc,_hd){return E(_hc)==0?E(_hd)==0?false:true:E(_hd)==0?true:false;},_he=function(_hf,_hg){return E(_hf)==0?E(_hg)==0?true:false:E(_hg)==0?false:true;},_hh=[0,_he,_hb],_hi=1,_hj=0,_hk=new T(function(){return B(unCStr("him"));}),_hl=new T(function(){return B(unCStr("her"));}),_hm=new T(function(){return B(unCStr("it"));}),_hn=0,_ho=function(_hp){var _hq=E(_hp);if(!_hq[0]){return [0];}else{var _hr=E(_hq[1]);return [1,[0,_hn,_hr[2],_hr[3]],new T(function(){return B(_N(_b,new T(function(){return B(_ho(_hq[2]));},1)));})];}},_hs=1,_ht=function(_hu){var _hv=E(_hu);if(!_hv[0]){return [0];}else{var _hw=E(_hv[1]);return [1,[0,_hs,_hw[2],_hw[3]],new T(function(){return B(_N(_b,new T(function(){return B(_ht(_hv[2]));},1)));})];}},_hx=2,_hy=function(_hz){var _hA=E(_hz);if(!_hA[0]){return [0];}else{var _hB=E(_hA[1]);return [1,[0,_hx,_hB[2],_hB[3]],new T(function(){return B(_N(_b,new T(function(){return B(_hy(_hA[2]));},1)));})];}},_hC=function(_hD,_hE){return new F(function(){return _N(B(_ho(B(_gN(_hk,_hD,_hE)))),new T(function(){return B(_N(B(_ht(B(_gN(_hl,_hD,_hE)))),new T(function(){return B(_hy(B(_gN(_hm,_hD,_hE))));},1)));},1));});},_hF=function(_hG,_hH,_hI){return new F(function(){return _hC(_hH,_hI);});},_hJ=new T(function(){return B(unCStr("he"));}),_hK=new T(function(){return B(unCStr("she"));}),_hL=function(_hM){var _hN=E(_hM);if(!_hN[0]){return [0];}else{var _hO=E(_hN[1]);return [1,[0,_hn,_hO[2],_hO[3]],new T(function(){return B(_N(_b,new T(function(){return B(_hL(_hN[2]));},1)));})];}},_hP=function(_hQ){var _hR=E(_hQ);if(!_hR[0]){return [0];}else{var _hS=E(_hR[1]);return [1,[0,_hs,_hS[2],_hS[3]],new T(function(){return B(_N(_b,new T(function(){return B(_hP(_hR[2]));},1)));})];}},_hT=function(_hU){var _hV=E(_hU);if(!_hV[0]){return [0];}else{var _hW=E(_hV[1]);return [1,[0,_hx,_hW[2],_hW[3]],new T(function(){return B(_N(_b,new T(function(){return B(_hT(_hV[2]));},1)));})];}},_hX=function(_hY,_hZ){return new F(function(){return _N(B(_hL(B(_gN(_hJ,_hY,_hZ)))),new T(function(){return B(_N(B(_hP(B(_gN(_hK,_hY,_hZ)))),new T(function(){return B(_hT(B(_gN(_hm,_hY,_hZ))));},1)));},1));});},_i0=function(_i1,_i2,_i3){return new F(function(){return _hX(_i2,_i3);});},_i4=new T(function(){return B(unCStr("temperature"));}),_i5=[0,_i4,_hx],_i6=[1,_i5,_b],_i7=new T(function(){return B(unCStr("price"));}),_i8=[0,_i7,_hx],_i9=[1,_i8,_i6],_ia=new T(function(){return B(unCStr("unicorn"));}),_ib=[0,_ia,_hx],_ic=[1,_ib,_i9],_id=new T(function(){return B(unCStr("pen"));}),_ie=[0,_id,_hx],_if=[1,_ie,_ic],_ig=new T(function(){return B(unCStr("fish"));}),_ih=[0,_ig,_hx],_ii=[1,_ih,_if],_ij=new T(function(){return B(unCStr("park"));}),_ik=[0,_ij,_hx],_il=[1,_ik,_ii],_im=new T(function(){return B(unCStr("woman"));}),_in=[0,_im,_hs],_io=[1,_in,_il],_ip=new T(function(){return B(unCStr("man"));}),_iq=[0,_ip,_hn],_ir=[1,_iq,_io],_is=function(_it,_iu,_iv){while(1){var _iw=E(_iv);if(!_iw[0]){return [0];}else{var _ix=E(_iw[1]);if(!B(A(_6j,[_it,_iu,_ix[1]]))){_iv=_iw[2];continue;}else{return [1,_ix[2]];}}}},_iy=function(_iz){while(1){var _iA=(function(_iB){var _iC=E(_iB);if(!_iC[0]){return [0];}else{var _iD=_iC[2],_iE=E(_iC[1]),_iF=_iE[1],_iG=B(_is(_5H,_iF,_ir));if(!_iG[0]){_iz=_iD;return null;}else{return [1,[0,[0,[0,_el,_iF],_iG[1]],_iE[2],_iE[3]],new T(function(){return B(_N(_b,new T(function(){return B(_iy(_iD));},1)));})];}}})(_iz);if(_iA!=null){return _iA;}}},_iH=new T(function(){return B(_iy(_b));}),_iI=function(_iJ,_iK){var _iL=E(_iK);return _iL[0]==0?E(_iH):B(_iy([1,[0,_iL[1],_iJ,_iL[2]],_b]));},_iM=function(_iN,_iO,_iP){return new F(function(){return _iI(_iO,_iP);});},_iQ=function(_iR){return (_iR>>>0^-1>>>0)>>>0&4294967295;},_iS=function(_iT,_iU){return new F(function(){return (function(_iV){while(1){var _iW=E(_iV);switch(_iW[0]){case 0:var _iX=_iW[2]>>>0;if(((_iT>>>0&((_iX-1>>>0^4294967295)>>>0^_iX)>>>0)>>>0&4294967295)==_iW[1]){if(!((_iT>>>0&_iX)>>>0)){_iV=_iW[3];continue;}else{_iV=_iW[4];continue;}}else{return false;}break;case 1:return E(new T(function(){return [0,(_iT>>>0&B(_iQ(31))>>>0)>>>0&4294967295];}))[1]!=_iW[1]?false:(E(new T(function(){return [0,1<<((_iT>>>0&31>>>0)>>>0&4294967295)>>>0];}))[1]&_iW[2])>>>0==0?false:true;default:return false;}}})(_iU);});},_iY=function(_iZ){var _j0=E(_iZ);if(!_j0[0]){return [0];}else{var _j1=E(_j0[1]);return [1,[0,E(_j1[1])[2],_j1[2],_j1[3]],new T(function(){return B(_N(_b,new T(function(){return B(_iY(_j0[2]));},1)));})];}},_j2=function(_j3,_j4,_j5){var _j6=function(_j7){while(1){var _j8=(function(_j9){var _ja=E(_j9);if(!_ja[0]){return [0];}else{var _jb=_ja[2],_jc=E(_ja[1]);if(!B((function(_jd){while(1){var _je=E(_jd);if(!_je[0]){return true;}else{if(!B(_iS(E(_j3)[1],E(_je[1])[4]))){_jd=_je[2];continue;}else{return false;}}}})(_jc[1]))){_j7=_jb;return null;}else{return [1,[0,_j,_jc[2],_jc[3]],new T(function(){return B(_N(_b,new T(function(){return B(_j6(_jb));},1)));})];}}})(_j7);if(_j8!=null){return _j8;}}};return new F(function(){return _j6(B(_iY([1,[0,_j4,_j4,_j5],_b])));});},_jf=new T(function(){return B(unCStr("such"));}),_jg=new T(function(){return B(unCStr("that"));}),_jh=function(_ji,_jj,_jk){while(1){var _jl=E(_jk);switch(_jl[0]){case 0:var _jm=_jl[1],_jn=_jl[2],_jo=_jl[3],_jp=_jl[4],_jq=_jn>>>0;if(((_ji>>>0&((_jq-1>>>0^4294967295)>>>0^_jq)>>>0)>>>0&4294967295)==_jm){if(!((_ji>>>0&_jq)>>>0)){var _jr=E(_jp);if(_jr[0]==2){_jk=_jo;continue;}else{var _js=B(_jh(_ji,_jj,_jo));return _js[0]==2?E(_jr):[0,_jm,_jn,E(_js),E(_jr)];}}else{var _jt=B(_jh(_ji,_jj,_jp));if(_jt[0]==2){return E(_jo);}else{var _ju=E(_jo);return _ju[0]==2?E(_jt):[0,_jm,_jn,E(_ju),E(_jt)];}}}else{return E(_jl);}break;case 1:if(_jl[1]!=_ji){return E(_jl);}else{var _jv=(_jl[2]&(_jj^4294967295)>>>0)>>>0;return _jv==0?[2]:[1,_ji,_jv];}break;default:return [2];}}},_jw=function(_jx){return function(_jy){return new F(function(){return _jh(E(new T(function(){return [0,(_jx>>>0&B(_iQ(31))>>>0)>>>0&4294967295];}))[1],E(new T(function(){return [0,1<<((_jx>>>0&31>>>0)>>>0&4294967295)>>>0];}))[1],_jy);});};},_jz=function(_jA){var _jB=E(_jA)[1]>>>0;return [1,(_jB&B(_iQ(31))>>>0)>>>0&4294967295,1<<((_jB&31>>>0)>>>0&4294967295)>>>0];},_jC=function(_jD,_jE,_jF){var _jG=E(_jF);switch(_jG[0]){case 0:var _jH=_jG[1],_jI=_jG[2],_jJ=_jG[3],_jK=_jG[4],_jL=_jI>>>0;if(((_jD>>>0&((_jL-1>>>0^4294967295)>>>0^_jL)>>>0)>>>0&4294967295)==_jH){return (_jD>>>0&_jL)>>>0==0?[0,_jH,_jI,E(B(_jC(_jD,_jE,_jJ))),E(_jK)]:[0,_jH,_jI,E(_jJ),E(B(_jC(_jD,_jE,_jK)))];}else{var _jM=(_jD>>>0^_jH>>>0)>>>0,_jN=(_jM|_jM>>>1)>>>0,_jO=(_jN|_jN>>>2)>>>0,_jP=(_jO|_jO>>>4)>>>0,_jQ=(_jP|_jP>>>8)>>>0,_jR=(_jQ|_jQ>>>16)>>>0,_jS=(_jR^_jR>>>1)>>>0&4294967295,_jT=_jS>>>0;return (_jD>>>0&_jT)>>>0==0?[0,(_jD>>>0&((_jT-1>>>0^4294967295)>>>0^_jT)>>>0)>>>0&4294967295,_jS,E([1,_jD,_jE]),E(_jG)]:[0,(_jD>>>0&((_jT-1>>>0^4294967295)>>>0^_jT)>>>0)>>>0&4294967295,_jS,E(_jG),E([1,_jD,_jE])];}break;case 1:var _jU=_jG[1];if(_jU!=_jD){var _jV=(_jD>>>0^_jU>>>0)>>>0,_jW=(_jV|_jV>>>1)>>>0,_jX=(_jW|_jW>>>2)>>>0,_jY=(_jX|_jX>>>4)>>>0,_jZ=(_jY|_jY>>>8)>>>0,_k0=(_jZ|_jZ>>>16)>>>0,_k1=(_k0^_k0>>>1)>>>0&4294967295,_k2=_k1>>>0;return (_jD>>>0&_k2)>>>0==0?[0,(_jD>>>0&((_k2-1>>>0^4294967295)>>>0^_k2)>>>0)>>>0&4294967295,_k1,E([1,_jD,_jE]),E(_jG)]:[0,(_jD>>>0&((_k2-1>>>0^4294967295)>>>0^_k2)>>>0)>>>0&4294967295,_k1,E(_jG),E([1,_jD,_jE])];}else{return [1,_jU,(_jE|_jG[2])>>>0];}break;default:return [1,_jD,_jE];}},_k3=function(_k4,_k5,_k6,_k7,_k8,_k9){var _ka=_k7>>>0;if(((_k4>>>0&((_ka-1>>>0^4294967295)>>>0^_ka)>>>0)>>>0&4294967295)==_k6){return (_k4>>>0&_ka)>>>0==0?[0,_k6,_k7,E(B(_jC(_k4,_k5,_k8))),E(_k9)]:[0,_k6,_k7,E(_k8),E(B(_jC(_k4,_k5,_k9)))];}else{var _kb=(_k4>>>0^_k6>>>0)>>>0,_kc=(_kb|_kb>>>1)>>>0,_kd=(_kc|_kc>>>2)>>>0,_ke=(_kd|_kd>>>4)>>>0,_kf=(_ke|_ke>>>8)>>>0,_kg=(_kf|_kf>>>16)>>>0,_kh=(_kg^_kg>>>1)>>>0&4294967295,_ki=_kh>>>0;return (_k4>>>0&_ki)>>>0==0?[0,(_k4>>>0&((_ki-1>>>0^4294967295)>>>0^_ki)>>>0)>>>0&4294967295,_kh,E([1,_k4,_k5]),E([0,_k6,_k7,E(_k8),E(_k9)])]:[0,(_k4>>>0&((_ki-1>>>0^4294967295)>>>0^_ki)>>>0)>>>0&4294967295,_kh,E([0,_k6,_k7,E(_k8),E(_k9)]),E([1,_k4,_k5])];}},_kj=function(_kk,_kl,_km,_kn,_ko){var _kp=E(_ko);switch(_kp[0]){case 0:var _kq=_kp[1],_kr=_kp[2],_ks=_kp[3],_kt=_kp[4];if(_kl>>>0<=_kr>>>0){if(_kr>>>0<=_kl>>>0){if(_kk!=_kq){var _ku=(_kk>>>0^_kq>>>0)>>>0,_kv=(_ku|_ku>>>1)>>>0,_kw=(_kv|_kv>>>2)>>>0,_kx=(_kw|_kw>>>4)>>>0,_ky=(_kx|_kx>>>8)>>>0,_kz=(_ky|_ky>>>16)>>>0,_kA=(_kz^_kz>>>1)>>>0&4294967295,_kB=_kA>>>0;return (_kk>>>0&_kB)>>>0==0?[0,(_kk>>>0&((_kB-1>>>0^4294967295)>>>0^_kB)>>>0)>>>0&4294967295,_kA,E([0,_kk,_kl,E(_km),E(_kn)]),E(_kp)]:[0,(_kk>>>0&((_kB-1>>>0^4294967295)>>>0^_kB)>>>0)>>>0&4294967295,_kA,E(_kp),E([0,_kk,_kl,E(_km),E(_kn)])];}else{return [0,_kk,_kl,E(B(_kC(_km,_ks))),E(B(_kC(_kn,_kt)))];}}else{var _kD=_kr>>>0;if(((_kk>>>0&((_kD-1>>>0^4294967295)>>>0^_kD)>>>0)>>>0&4294967295)==_kq){return (_kk>>>0&_kD)>>>0==0?[0,_kq,_kr,E(B(_kj(_kk,_kl,_km,_kn,_ks))),E(_kt)]:[0,_kq,_kr,E(_ks),E(B(_kj(_kk,_kl,_km,_kn,_kt)))];}else{var _kE=(_kk>>>0^_kq>>>0)>>>0,_kF=(_kE|_kE>>>1)>>>0,_kG=(_kF|_kF>>>2)>>>0,_kH=(_kG|_kG>>>4)>>>0,_kI=(_kH|_kH>>>8)>>>0,_kJ=(_kI|_kI>>>16)>>>0,_kK=(_kJ^_kJ>>>1)>>>0&4294967295,_kL=_kK>>>0;return (_kk>>>0&_kL)>>>0==0?[0,(_kk>>>0&((_kL-1>>>0^4294967295)>>>0^_kL)>>>0)>>>0&4294967295,_kK,E([0,_kk,_kl,E(_km),E(_kn)]),E(_kp)]:[0,(_kk>>>0&((_kL-1>>>0^4294967295)>>>0^_kL)>>>0)>>>0&4294967295,_kK,E(_kp),E([0,_kk,_kl,E(_km),E(_kn)])];}}}else{var _kM=_kl>>>0;if(((_kq>>>0&((_kM-1>>>0^4294967295)>>>0^_kM)>>>0)>>>0&4294967295)==_kk){return (_kq>>>0&_kM)>>>0==0?[0,_kk,_kl,E(B(_kN(_km,_kq,_kr,_ks,_kt))),E(_kn)]:[0,_kk,_kl,E(_km),E(B(_kN(_kn,_kq,_kr,_ks,_kt)))];}else{var _kO=(_kk>>>0^_kq>>>0)>>>0,_kP=(_kO|_kO>>>1)>>>0,_kQ=(_kP|_kP>>>2)>>>0,_kR=(_kQ|_kQ>>>4)>>>0,_kS=(_kR|_kR>>>8)>>>0,_kT=(_kS|_kS>>>16)>>>0,_kU=(_kT^_kT>>>1)>>>0&4294967295,_kV=_kU>>>0;return (_kk>>>0&_kV)>>>0==0?[0,(_kk>>>0&((_kV-1>>>0^4294967295)>>>0^_kV)>>>0)>>>0&4294967295,_kU,E([0,_kk,_kl,E(_km),E(_kn)]),E(_kp)]:[0,(_kk>>>0&((_kV-1>>>0^4294967295)>>>0^_kV)>>>0)>>>0&4294967295,_kU,E(_kp),E([0,_kk,_kl,E(_km),E(_kn)])];}}break;case 1:return new F(function(){return _k3(_kp[1],_kp[2],_kk,_kl,_km,_kn);});break;default:return [0,_kk,_kl,E(_km),E(_kn)];}},_kN=function(_kW,_kX,_kY,_kZ,_l0){var _l1=E(_kW);switch(_l1[0]){case 0:var _l2=_l1[1],_l3=_l1[2],_l4=_l1[3],_l5=_l1[4];if(_l3>>>0<=_kY>>>0){if(_kY>>>0<=_l3>>>0){if(_l2!=_kX){var _l6=(_l2>>>0^_kX>>>0)>>>0,_l7=(_l6|_l6>>>1)>>>0,_l8=(_l7|_l7>>>2)>>>0,_l9=(_l8|_l8>>>4)>>>0,_la=(_l9|_l9>>>8)>>>0,_lb=(_la|_la>>>16)>>>0,_lc=(_lb^_lb>>>1)>>>0&4294967295,_ld=_lc>>>0;return (_l2>>>0&_ld)>>>0==0?[0,(_l2>>>0&((_ld-1>>>0^4294967295)>>>0^_ld)>>>0)>>>0&4294967295,_lc,E(_l1),E([0,_kX,_kY,E(_kZ),E(_l0)])]:[0,(_l2>>>0&((_ld-1>>>0^4294967295)>>>0^_ld)>>>0)>>>0&4294967295,_lc,E([0,_kX,_kY,E(_kZ),E(_l0)]),E(_l1)];}else{return [0,_l2,_l3,E(B(_kC(_l4,_kZ))),E(B(_kC(_l5,_l0)))];}}else{var _le=_kY>>>0;if(((_l2>>>0&((_le-1>>>0^4294967295)>>>0^_le)>>>0)>>>0&4294967295)==_kX){return (_l2>>>0&_le)>>>0==0?[0,_kX,_kY,E(B(_kj(_l2,_l3,_l4,_l5,_kZ))),E(_l0)]:[0,_kX,_kY,E(_kZ),E(B(_kj(_l2,_l3,_l4,_l5,_l0)))];}else{var _lf=(_l2>>>0^_kX>>>0)>>>0,_lg=(_lf|_lf>>>1)>>>0,_lh=(_lg|_lg>>>2)>>>0,_li=(_lh|_lh>>>4)>>>0,_lj=(_li|_li>>>8)>>>0,_lk=(_lj|_lj>>>16)>>>0,_ll=(_lk^_lk>>>1)>>>0&4294967295,_lm=_ll>>>0;return (_l2>>>0&_lm)>>>0==0?[0,(_l2>>>0&((_lm-1>>>0^4294967295)>>>0^_lm)>>>0)>>>0&4294967295,_ll,E(_l1),E([0,_kX,_kY,E(_kZ),E(_l0)])]:[0,(_l2>>>0&((_lm-1>>>0^4294967295)>>>0^_lm)>>>0)>>>0&4294967295,_ll,E([0,_kX,_kY,E(_kZ),E(_l0)]),E(_l1)];}}}else{var _ln=_l3>>>0;if(((_kX>>>0&((_ln-1>>>0^4294967295)>>>0^_ln)>>>0)>>>0&4294967295)==_l2){return (_kX>>>0&_ln)>>>0==0?[0,_l2,_l3,E(B(_kN(_l4,_kX,_kY,_kZ,_l0))),E(_l5)]:[0,_l2,_l3,E(_l4),E(B(_kN(_l5,_kX,_kY,_kZ,_l0)))];}else{var _lo=(_l2>>>0^_kX>>>0)>>>0,_lp=(_lo|_lo>>>1)>>>0,_lq=(_lp|_lp>>>2)>>>0,_lr=(_lq|_lq>>>4)>>>0,_ls=(_lr|_lr>>>8)>>>0,_lt=(_ls|_ls>>>16)>>>0,_lu=(_lt^_lt>>>1)>>>0&4294967295,_lv=_lu>>>0;return (_l2>>>0&_lv)>>>0==0?[0,(_l2>>>0&((_lv-1>>>0^4294967295)>>>0^_lv)>>>0)>>>0&4294967295,_lu,E(_l1),E([0,_kX,_kY,E(_kZ),E(_l0)])]:[0,(_l2>>>0&((_lv-1>>>0^4294967295)>>>0^_lv)>>>0)>>>0&4294967295,_lu,E([0,_kX,_kY,E(_kZ),E(_l0)]),E(_l1)];}}break;case 1:return new F(function(){return _k3(_l1[1],_l1[2],_kX,_kY,_kZ,_l0);});break;default:return [0,_kX,_kY,E(_kZ),E(_l0)];}},_kC=function(_lw,_lx){var _ly=E(_lw);switch(_ly[0]){case 0:var _lz=_ly[1],_lA=_ly[2],_lB=_ly[3],_lC=_ly[4],_lD=E(_lx);switch(_lD[0]){case 0:var _lE=_lD[1],_lF=_lD[2],_lG=_lD[3],_lH=_lD[4];if(_lA>>>0<=_lF>>>0){if(_lF>>>0<=_lA>>>0){if(_lz!=_lE){var _lI=(_lz>>>0^_lE>>>0)>>>0,_lJ=(_lI|_lI>>>1)>>>0,_lK=(_lJ|_lJ>>>2)>>>0,_lL=(_lK|_lK>>>4)>>>0,_lM=(_lL|_lL>>>8)>>>0,_lN=(_lM|_lM>>>16)>>>0,_lO=(_lN^_lN>>>1)>>>0&4294967295,_lP=_lO>>>0;return (_lz>>>0&_lP)>>>0==0?[0,(_lz>>>0&((_lP-1>>>0^4294967295)>>>0^_lP)>>>0)>>>0&4294967295,_lO,E(_ly),E(_lD)]:[0,(_lz>>>0&((_lP-1>>>0^4294967295)>>>0^_lP)>>>0)>>>0&4294967295,_lO,E(_lD),E(_ly)];}else{return [0,_lz,_lA,E(B(_kC(_lB,_lG))),E(B(_kC(_lC,_lH)))];}}else{var _lQ=_lF>>>0;if(((_lz>>>0&((_lQ-1>>>0^4294967295)>>>0^_lQ)>>>0)>>>0&4294967295)==_lE){return (_lz>>>0&_lQ)>>>0==0?[0,_lE,_lF,E(B(_kj(_lz,_lA,_lB,_lC,_lG))),E(_lH)]:[0,_lE,_lF,E(_lG),E(B(_kj(_lz,_lA,_lB,_lC,_lH)))];}else{var _lR=(_lz>>>0^_lE>>>0)>>>0,_lS=(_lR|_lR>>>1)>>>0,_lT=(_lS|_lS>>>2)>>>0,_lU=(_lT|_lT>>>4)>>>0,_lV=(_lU|_lU>>>8)>>>0,_lW=(_lV|_lV>>>16)>>>0,_lX=(_lW^_lW>>>1)>>>0&4294967295,_lY=_lX>>>0;return (_lz>>>0&_lY)>>>0==0?[0,(_lz>>>0&((_lY-1>>>0^4294967295)>>>0^_lY)>>>0)>>>0&4294967295,_lX,E(_ly),E(_lD)]:[0,(_lz>>>0&((_lY-1>>>0^4294967295)>>>0^_lY)>>>0)>>>0&4294967295,_lX,E(_lD),E(_ly)];}}}else{var _lZ=_lA>>>0;if(((_lE>>>0&((_lZ-1>>>0^4294967295)>>>0^_lZ)>>>0)>>>0&4294967295)==_lz){return (_lE>>>0&_lZ)>>>0==0?[0,_lz,_lA,E(B(_kN(_lB,_lE,_lF,_lG,_lH))),E(_lC)]:[0,_lz,_lA,E(_lB),E(B(_kN(_lC,_lE,_lF,_lG,_lH)))];}else{var _m0=(_lz>>>0^_lE>>>0)>>>0,_m1=(_m0|_m0>>>1)>>>0,_m2=(_m1|_m1>>>2)>>>0,_m3=(_m2|_m2>>>4)>>>0,_m4=(_m3|_m3>>>8)>>>0,_m5=(_m4|_m4>>>16)>>>0,_m6=(_m5^_m5>>>1)>>>0&4294967295,_m7=_m6>>>0;return (_lz>>>0&_m7)>>>0==0?[0,(_lz>>>0&((_m7-1>>>0^4294967295)>>>0^_m7)>>>0)>>>0&4294967295,_m6,E(_ly),E(_lD)]:[0,(_lz>>>0&((_m7-1>>>0^4294967295)>>>0^_m7)>>>0)>>>0&4294967295,_m6,E(_lD),E(_ly)];}}break;case 1:return new F(function(){return _k3(_lD[1],_lD[2],_lz,_lA,_lB,_lC);});break;default:return E(_ly);}break;case 1:return new F(function(){return _jC(_ly[1],_ly[2],_lx);});break;default:return E(_lx);}},_m8=function(_m9){while(1){var _ma=(function(_mb){var _mc=E(_mb);switch(_mc[0]){case 0:return [2];case 1:return new F(function(){return _jz(_mc[2]);});break;case 2:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 3:return new F(function(){return A(_jw,[E(_mc[2])[1],new T(function(){return B(_kC(B(_m8(_mc[3])),B(_m8(_mc[4]))));})]);});break;case 4:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 5:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 6:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 7:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 8:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 9:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 10:return new F(function(){return A(_jw,[E(_mc[2])[1],new T(function(){return B(_kC(B(_m8(_mc[3])),B(_m8(_mc[4]))));})]);});break;case 11:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 12:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 13:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 14:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 15:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 16:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 17:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 18:_m9=_mc[2];return null;case 19:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 20:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 21:_m9=_mc[2];return null;case 22:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;case 23:return new F(function(){return _kC(B(_m8(_mc[2])),B(_m8(_mc[3])));});break;default:_m9=_mc[2];return null;}})(_m9);if(_ma!=null){return _ma;}}},_md=function(_me,_mf,_mg){var _mh=function(_mi){var _mj=E(_mi);if(!_mj[0]){return [0];}else{var _mk=E(_mj[1]);return [1,[0,_me,_mk[2],_mk[3]],new T(function(){return B(_N(_b,new T(function(){return B(_mh(_mj[2]));},1)));})];}};return new F(function(){return _mh([1,[0,_j,[0,new T(function(){return [0,E(_me)[1]+1|0];}),_mf],_mg],_b]);});},_ml=function(_mm){while(1){var _mn=(function(_mo){var _mp=E(_mo);if(!_mp[0]){return [0];}else{var _mq=_mp[2],_mr=E(_mp[1]),_ms=E(_mr[1]),_mt=B(_md(_ms[1],_ms[2],_mr[3]));if(!_mt[0]){_mm=_mq;return null;}else{return [1,_mt[1],new T(function(){return B(_N(_mt[2],new T(function(){return B(_ml(_mq));},1)));})];}}})(_mm);if(_mn!=null){return _mn;}}},_mu=function(_mv,_mw,_mx){var _my=function(_mz){var _mA=E(_mz);if(!_mA[0]){return [0];}else{var _mB=E(_mA[1]);return [1,[0,_j,new T(function(){return [0,E(_mB[1])[1],_mv];}),_mB[3]],new T(function(){return B(_N(_b,new T(function(){return B(_my(_mA[2]));},1)));})];}};return new F(function(){return _my([1,[0,_mw,_mw,_mx],_b]);});},_mC=function(_mD){var _mE=E(_mD);if(!_mE[0]){return [0];}else{var _mF=E(_mE[1]);return [1,[0,E(_mF[1])[2],_mF[2],_mF[3]],new T(function(){return B(_N(_b,new T(function(){return B(_mC(_mE[2]));},1)));})];}},_mG=function(_mH,_mI,_mJ){var _mK=E(_mH);if(!_mK[0]){return [0];}else{return new F(function(){return _N([1,[0,_mK[1],_mI,_mJ],_b],new T(function(){return B(_mG(_mK[2],_mI,_mJ));},1));});}},_mL=function(_mM){while(1){var _mN=(function(_mO){var _mP=E(_mO);if(!_mP[0]){return [0];}else{var _mQ=_mP[2],_mR=E(_mP[1]),_mS=E(_mR[1]),_mT=E(_mS[1]),_mU=function(_mV){var _mW=E(_mV);if(!_mW[0]){return [0];}else{var _mX=E(_mW[1]);return [1,[0,_mT,_mX[2],_mX[3]],new T(function(){return B(_N(_b,new T(function(){return B(_mU(_mW[2]));},1)));})];}},_mY=function(_mZ){while(1){var _n0=(function(_n1){var _n2=E(_n1);if(!_n2[0]){return [0];}else{var _n3=_n2[2],_n4=E(_n2[1]),_n5=B(_mU(B(_mu(_mS[2],_n4[2],_n4[3]))));if(!_n5[0]){_mZ=_n3;return null;}else{return [1,_n5[1],new T(function(){return B(_N(_n5[2],new T(function(){return B(_mY(_n3));},1)));})];}}})(_mZ);if(_n0!=null){return _n0;}}},_n6=B(_mY(B(_j2(_mT[1],_mR[2],_mR[3]))));if(!_n6[0]){_mM=_mQ;return null;}else{return [1,_n6[1],new T(function(){return B(_N(_n6[2],new T(function(){return B(_mL(_mQ));},1)));})];}}})(_mM);if(_mN!=null){return _mN;}}},_n7=function(_n8){var _n9=E(_n8);if(!_n9[0]){return [0];}else{var _na=_n9[1],_nb=_n9[2];return [1,[0,_na,_nb],new T(function(){var _nc=function(_nd){var _ne=E(_nd);if(!_ne[0]){return [0];}else{var _nf=E(_ne[1]);return [1,[0,_nf[1],[1,_na,_nf[2]]],new T(function(){return B(_nc(_ne[2]));})];}};return B(_nc(B(_n7(_nb))));})];}},_ng=function(_nh){while(1){var _ni=(function(_nj){var _nk=E(_nj);if(!_nk[0]){return [0];}else{var _nl=_nk[2],_nm=E(_nk[1]),_nn=B(_mL(B(_mG(B(_n7(_nm[1])),_nm[2],_nm[3]))));if(!_nn[0]){_nh=_nl;return null;}else{return [1,_nn[1],new T(function(){return B(_N(_nn[2],new T(function(){return B(_ng(_nl));},1)));})];}}})(_nh);if(_ni!=null){return _ni;}}},_no=function(_np,_nq,_nr,_ns){var _nt=function(_nu){while(1){var _nv=(function(_nw){var _nx=E(_nw);if(!_nx[0]){return [0];}else{var _ny=_nx[2],_nz=E(_nx[1]),_nA=B(_no(_np,_nz[1],_nz[2],_nz[3]));if(!_nA[0]){_nu=_ny;return null;}else{return [1,_nA[1],new T(function(){return B(_N(_nA[2],new T(function(){return B(_nt(_ny));},1)));})];}}})(_nu);if(_nv!=null){return _nv;}}},_nB=function(_nC){var _nD=E(_nC);if(!_nD[0]){return [0];}else{var _nE=E(_nD[1]),_nF=E(_nE[1]);return [1,[0,[10,_np,_nF[1],_nF[3],_nq],_nE[2],_nE[3]],new T(function(){return B(_N(_b,new T(function(){return B(_nB(_nD[2]));},1)));})];}};return new F(function(){return _N(B(_nt(B(_nB(B(_ng(B(_mC([1,[0,_nr,_nr,_ns],_b])))))))),[1,[0,_nq,_nr,_ns],_b]);});},_nG=function(_nH){var _nI=E(_nH);if(!_nI[0]){return [0];}else{var _nJ=E(_nI[1]);return [1,[0,E(_nJ[1])[2],_nJ[2],_nJ[3]],new T(function(){return B(_N(_b,new T(function(){return B(_nG(_nI[2]));},1)));})];}},_nK=function(_nL){var _nM=E(_nL);if(!_nM[0]){return [0];}else{var _nN=E(_nM[1]);return [1,[0,E(_nN[1])[2],_nN[2],_nN[3]],new T(function(){return B(_N(_b,new T(function(){return B(_nK(_nM[2]));},1)));})];}},_nO=function(_nP){var _nQ=E(_nP);if(!_nQ[0]){return [0];}else{var _nR=E(_nQ[1]);return [1,[0,_nR[1],_nR[2]],new T(function(){return B(_nO(_nQ[2]));})];}},_nS=function(_nT,_nU,_nV,_nW,_nX){var _nY=function(_nZ){while(1){var _o0=(function(_o1){var _o2=E(_o1);if(!_o2[0]){return [0];}else{var _o3=_o2[2],_o4=E(_o2[1]),_o5=_o4[1],_o6=function(_o7){while(1){var _o8=(function(_o9){var _oa=E(_o9);if(!_oa[0]){return [0];}else{var _ob=_oa[2],_oc=E(_oa[1]),_od=E(_oc[1]),_oe=function(_of){while(1){var _og=(function(_oh){var _oi=E(_oh);if(!_oi[0]){return [0];}else{var _oj=_oi[2],_ok=E(_oi[1]),_ol=_ok[2],_om=function(_on){var _oo=E(_on);if(!_oo[0]){return [0];}else{var _op=E(_oo[1]);return [1,[0,[0,_ok[1],_od[2]],_op[2],_op[3]],new T(function(){return B(_N(_b,new T(function(){return B(_om(_oo[2]));},1)));})];}},_oq=function(_or){while(1){var _os=(function(_ot){var _ou=E(_ot);if(!_ou[0]){return [0];}else{var _ov=_ou[2],_ow=E(_ou[1]),_ox=B(_om(B(_mu(new T(function(){return B(_N(_ow[1],_o5));}),_ow[2],_ow[3]))));if(!_ox[0]){_or=_ov;return null;}else{return [1,_ox[1],new T(function(){return B(_N(_ox[2],new T(function(){return B(_oq(_ov));},1)));})];}}})(_or);if(_os!=null){return _os;}}},_oy=B(_oq(B(_nG([1,[0,_ol,_ol,_ok[3]],_b]))));if(!_oy[0]){_of=_oj;return null;}else{return [1,_oy[1],new T(function(){return B(_N(_oy[2],new T(function(){return B(_oe(_oj));},1)));})];}}})(_of);if(_og!=null){return _og;}}},_oz=B(_oe(B(_no(_nT,_od[1],_oc[2],_oc[3]))));if(!_oz[0]){_o7=_ob;return null;}else{return [1,_oz[1],new T(function(){return B(_N(_oz[2],new T(function(){return B(_o6(_ob));},1)));})];}}})(_o7);if(_o8!=null){return _o8;}}},_oA=function(_oB){while(1){var _oC=(function(_oD){var _oE=E(_oD);if(!_oE[0]){return [0];}else{var _oF=_oE[2],_oG=E(_oE[1]),_oH=B(_o6(B(A(_nU,[new T(function(){return [0,new T(function(){return B(_N(B(_nO(_o5)),E(_nV)[1]));})];}),_oG[2],_oG[3]]))));if(!_oH[0]){_oB=_oF;return null;}else{return [1,_oH[1],new T(function(){return B(_N(_oH[2],new T(function(){return B(_oA(_oF));},1)));})];}}})(_oB);if(_oC!=null){return _oC;}}},_oI=B(_oA(B(_mu(_b,_o4[2],_o4[3]))));if(!_oI[0]){_nZ=_o3;return null;}else{return [1,_oI[1],new T(function(){return B(_N(_oI[2],new T(function(){return B(_nY(_o3));},1)));})];}}})(_nZ);if(_o0!=null){return _o0;}}};return new F(function(){return _nY(B(_nK([1,[0,_nW,_nW,_nX],_b])));});},_oJ=function(_oK){var _oL=E(_oK);if(!_oL[0]){return [0];}else{var _oM=E(_oL[1]);return [1,[0,new T(function(){return E(E(_oM[1])[1]);}),_oM[2],_oM[3]],new T(function(){return B(_N(_b,new T(function(){return B(_oJ(_oL[2]));},1)));})];}},_oN=new T(function(){return B(unCStr("or"));}),_oO=new T(function(){return B(unCStr("and"));}),_oP=function(_oQ,_oR){return [9,_7x,_oQ,_oR];},_oS=function(_oT){var _oU=E(_oT);if(!_oU[0]){return [0];}else{var _oV=E(_oU[1]);return [1,[0,_oP,_oV[2],_oV[3]],new T(function(){return B(_N(_b,new T(function(){return B(_oS(_oU[2]));},1)));})];}},_oW=function(_oQ,_oR){return [8,_7x,_oQ,_oR];},_oX=function(_oY){var _oZ=E(_oY);if(!_oZ[0]){return [0];}else{var _p0=E(_oZ[1]);return [1,[0,_oW,_p0[2],_p0[3]],new T(function(){return B(_N(_b,new T(function(){return B(_oX(_oZ[2]));},1)));})];}},_p1=function(_p2,_p3){return new F(function(){return _N(B(_oX(B(_gN(_oO,_p2,_p3)))),new T(function(){return B(_oS(B(_gN(_oN,_p2,_p3))));},1));});},_p4=function(_p5,_p6,_p7){return new F(function(){return _p1(_p6,_p7);});},_p8=[0,_7x,_7x],_p9=[4,E(_5x)],_pa=[4,E(_5y)],_pb=[3,E(_5x)],_pc=[3,E(_5y)],_pd=[2,E(_5x)],_pe=[1,_hj,_b],_pf=new T(function(){return B(_pg(_pe));}),_ph=[3,_],_pi=[0,_7p,_ph],_pj=[0],_pk=function(_oQ,_oR){return [8,_7p,_oQ,_oR];},_pl=function(_pm){var _pn=E(_pm);if(!_pn[0]){return [0];}else{var _po=E(_pn[1]);return [1,[0,_pk,_po[2],_po[3]],new T(function(){return B(_N(_b,new T(function(){return B(_pl(_pn[2]));},1)));})];}},_pp=function(_oQ,_oR){return [9,_7p,_oQ,_oR];},_pq=function(_pr){var _ps=E(_pr);if(!_ps[0]){return [0];}else{var _pt=E(_ps[1]);return [1,[0,_pp,_pt[2],_pt[3]],new T(function(){return B(_N(_b,new T(function(){return B(_pq(_ps[2]));},1)));})];}},_pu=function(_pv,_pw){return new F(function(){return _N(B(_pl(B(_gN(_oO,_pv,_pw)))),new T(function(){return B(_pq(B(_gN(_oN,_pv,_pw))));},1));});},_px=function(_py,_pz,_pA){return new F(function(){return _pu(_pz,_pA);});},_pB=function(_pC){var _pD=E(_pC);if(!_pD[0]){return [0];}else{var _pE=E(_pD[1]);return [1,[0,function(_oR){return [1,_pE[1],_oR];},_pE[2],_pE[3]],new T(function(){return B(_N(_b,new T(function(){return B(_pB(_pD[2]));},1)));})];}},_pF=function(_pG,_pH,_pI,_pJ){var _pK=function(_pL,_pM,_pN){var _pO=function(_pP){while(1){var _pQ=(function(_pR){var _pS=E(_pR);if(!_pS[0]){return [0];}else{var _pT=_pS[2],_pU=E(_pS[1]),_pV=function(_pW){var _pX=E(_pW);if(!_pX[0]){return [0];}else{var _pY=E(_pX[1]);return [1,[0,new T(function(){return B(A(_pU[1],[_pY[1]]));}),_pY[2],_pY[3]],new T(function(){return B(_N(_b,new T(function(){return B(_pV(_pX[2]));},1)));})];}},_pZ=B(_pV(B(_pK(_pL,_pU[2],_pU[3]))));if(!_pZ[0]){_pP=_pT;return null;}else{return [1,_pZ[1],new T(function(){return B(_N(_pZ[2],new T(function(){return B(_pO(_pT));},1)));})];}}})(_pP);if(_pQ!=null){return _pQ;}}};return new F(function(){return _N(B(_pO(B(_pB(B(A(_pG,[_pL,_pM,_pN])))))),[1,[0,_b,_pM,_pN],_b]);});};return new F(function(){return _pK(_pH,_pI,_pJ);});},_q0=[4,_,_7p,_7p],_q1=[4,_,_7p,_7p],_q2=[4,_,_q1,_gK],_q3=new T(function(){return B(unCStr("in"));}),_q4=new T(function(){return B(unCStr("about"));}),_q5=[1,_q4,_b],_q6=[1,_q3,_q5],_q7=function(_q8){while(1){var _q9=(function(_qa){var _qb=E(_qa);if(!_qb[0]){return [0];}else{var _qc=_qb[2],_qd=E(_qb[1]),_qe=_qd[1],_qf=function(_qg){var _qh=E(_qg);if(!_qh[0]){return [0];}else{var _qi=E(_qh[1]);return [1,[0,[0,_q2,_qe],_qi[2],_qi[3]],new T(function(){return B(_N(_b,new T(function(){return B(_qf(_qh[2]));},1)));})];}};if(!B(_6l(_5H,_qe,_q6))){var _qj=B(_qf(_b));if(!_qj[0]){_q8=_qc;return null;}else{return [1,_qj[1],new T(function(){return B(_N(_qj[2],new T(function(){return B(_q7(_qc));},1)));})];}}else{var _qk=B(_qf([1,[0,_j,_qd[2],_qd[3]],_b]));if(!_qk[0]){_q8=_qc;return null;}else{return [1,_qk[1],new T(function(){return B(_N(_qk[2],new T(function(){return B(_q7(_qc));},1)));})];}}}})(_q8);if(_q9!=null){return _q9;}}},_ql=new T(function(){return B(unCStr("allegedly"));}),_qm=[1,_ql,_b],_qn=new T(function(){return B(unCStr("voluntarily"));}),_qo=[1,_qn,_qm],_qp=new T(function(){return B(unCStr("slowly"));}),_qq=[1,_qp,_qo],_qr=new T(function(){return B(unCStr("rapidly"));}),_qs=[1,_qr,_qq],_qt=function(_qu){while(1){var _qv=(function(_qw){var _qx=E(_qw);if(!_qx[0]){return [0];}else{var _qy=_qx[2],_qz=E(_qx[1]),_qA=_qz[1],_qB=function(_qC){var _qD=E(_qC);if(!_qD[0]){return [0];}else{var _qE=E(_qD[1]);return [1,[0,[0,_q1,_qA],_qE[2],_qE[3]],new T(function(){return B(_N(_b,new T(function(){return B(_qB(_qD[2]));},1)));})];}};if(!B(_6l(_5H,_qA,_qs))){var _qF=B(_qB(_b));if(!_qF[0]){_qu=_qy;return null;}else{return [1,_qF[1],new T(function(){return B(_N(_qF[2],new T(function(){return B(_qt(_qy));},1)));})];}}else{var _qG=B(_qB([1,[0,_j,_qz[2],_qz[3]],_b]));if(!_qG[0]){_qu=_qy;return null;}else{return [1,_qG[1],new T(function(){return B(_N(_qG[2],new T(function(){return B(_qt(_qy));},1)));})];}}}})(_qu);if(_qv!=null){return _qv;}}},_qH=new T(function(){return B(_q7(_b));}),_qI=new T(function(){return B(_qt(_b));}),_qJ=function(_qK,_qL,_qM){var _qN=new T(function(){var _qO=function(_qP){while(1){var _qQ=(function(_qR){var _qS=E(_qR);if(!_qS[0]){return [0];}else{var _qT=_qS[2],_qU=E(_qS[1]),_qV=function(_qW){var _qX=E(_qW);if(!_qX[0]){return [0];}else{var _qY=E(_qX[1]);return [1,[0,[5,_q0,_qU[1],_qY[1]],_qY[2],_qY[3]],new T(function(){return B(_N(_b,new T(function(){return B(_qV(_qX[2]));},1)));})];}},_qZ=B(_qV(B(A(_r0,[_qK,_qU[2],_qU[3]]))));if(!_qZ[0]){_qP=_qT;return null;}else{return [1,_qZ[1],new T(function(){return B(_N(_qZ[2],new T(function(){return B(_qO(_qT));},1)));})];}}})(_qP);if(_qQ!=null){return _qQ;}}},_r1=E(_qM);if(!_r1[0]){var _r2=B(_qO(_qH));}else{var _r2=B(_qO(B(_q7([1,[0,_r1[1],_qL,_r1[2]],_b]))));}return _r2;},1),_r3=E(_qM);if(!_r3[0]){return new F(function(){return _N(_qI,_qN);});}else{return new F(function(){return _N(B(_qt([1,[0,_r3[1],_qL,_r3[2]],_b])),_qN);});}},_r4=function(_r5,_r6){return [7,_,_r5,_r6];},_r7=function(_r8,_r9){while(1){var _ra=(function(_rb,_rc){var _rd=E(_rc);if(!_rd[0]){return E(_rb);}else{_r8=new T(function(){return B(_r4(_rd[1],_rb));});_r9=_rd[2];return null;}})(_r8,_r9);if(_ra!=null){return _ra;}}},_re=function(_rf,_rg,_rh,_ri){var _rj=function(_rk){var _rl=E(_rk);if(!_rl[0]){return [0];}else{var _rm=E(_rl[1]);return [1,[0,new T(function(){return B(_r7(_rf,_rm[1]));}),_rm[2],_rm[3]],new T(function(){return B(_N(_b,new T(function(){return B(_rj(_rl[2]));},1)));})];}};return new F(function(){return _rj(B(_pF(_qJ,_rg,_rh,_ri)));});},_rn=function(_ro,_rp,_rq,_rr){var _rs=function(_rt){var _ru=E(_rt);if(!_ru[0]){return [0];}else{var _rv=E(_ru[1]);return [1,[0,[23,_,_ro,_rv[1]],_rv[2],_rv[3]],new T(function(){return B(_N(_b,new T(function(){return B(_rs(_ru[2]));},1)));})];}};return new F(function(){return _rs(B(A(_r0,[_rp,_rq,_rr])));});},_rw=[0,115],_rx=[1,_rw,_b],_ry=new T(function(){return B(unCStr("es"));}),_rz=new T(function(){return B(unCStr("ies"));}),_rA=new T(function(){return B(unCStr("walk"));}),_rB=new T(function(){return B(_aP(_rA,_b));}),_rC=new T(function(){var _rD=E(_rB);if(!_rD[0]){var _rE=B(_N(_rA,_rx));}else{var _rF=_rD[2];switch(E(E(_rD[1])[1])){case 104:var _rG=E(_rF);if(!_rG[0]){var _rH=B(_N(_rA,_rx));}else{switch(E(E(_rG[1])[1])){case 99:var _rI=B(_N(_rA,_ry));break;case 115:var _rI=B(_N(_rA,_ry));break;default:var _rI=B(_N(_rA,_rx));}var _rJ=_rI,_rH=_rJ;}var _rK=_rH;break;case 115:var _rL=E(_rF);if(!_rL[0]){var _rM=B(_N(_rA,_rx));}else{var _rM=E(E(_rL[1])[1])==115?B(_N(_rA,_ry)):B(_N(_rA,_rx));}var _rK=_rM;break;case 120:var _rK=B(_N(_rA,_ry));break;case 121:var _rK=B(_N(B(_aP(_rF,_b)),_rz));break;default:var _rK=B(_N(_rA,_rx));}var _rN=_rK,_rE=_rN;}return _rE;}),_rO=new T(function(){return B(unCStr("ed"));}),_rP=[0,100],_rQ=[1,_rP,_b],_rR=new T(function(){return B(unCStr("ied"));}),_rS=new T(function(){var _rT=E(_rB);if(!_rT[0]){var _rU=B(_N(_rA,_rO));}else{switch(E(E(_rT[1])[1])){case 101:var _rV=B(_N(_rA,_rQ));break;case 121:var _rV=B(_N(B(_aP(_rT[2],_b)),_rR));break;default:var _rV=B(_N(_rA,_rO));}var _rW=_rV,_rU=_rW;}return _rU;}),_rX=[0,_rA,_rC,_rS],_rY=new T(function(){return B(unCStr("talk"));}),_rZ=new T(function(){return B(_aP(_rY,_b));}),_s0=new T(function(){var _s1=E(_rZ);if(!_s1[0]){var _s2=B(_N(_rY,_rx));}else{var _s3=_s1[2];switch(E(E(_s1[1])[1])){case 104:var _s4=E(_s3);if(!_s4[0]){var _s5=B(_N(_rY,_rx));}else{switch(E(E(_s4[1])[1])){case 99:var _s6=B(_N(_rY,_ry));break;case 115:var _s6=B(_N(_rY,_ry));break;default:var _s6=B(_N(_rY,_rx));}var _s7=_s6,_s5=_s7;}var _s8=_s5;break;case 115:var _s9=E(_s3);if(!_s9[0]){var _sa=B(_N(_rY,_rx));}else{var _sa=E(E(_s9[1])[1])==115?B(_N(_rY,_ry)):B(_N(_rY,_rx));}var _s8=_sa;break;case 120:var _s8=B(_N(_rY,_ry));break;case 121:var _s8=B(_N(B(_aP(_s3,_b)),_rz));break;default:var _s8=B(_N(_rY,_rx));}var _sb=_s8,_s2=_sb;}return _s2;}),_sc=new T(function(){var _sd=E(_rZ);if(!_sd[0]){var _se=B(_N(_rY,_rO));}else{switch(E(E(_sd[1])[1])){case 101:var _sf=B(_N(_rY,_rQ));break;case 121:var _sf=B(_N(B(_aP(_sd[2],_b)),_rR));break;default:var _sf=B(_N(_rY,_rO));}var _sg=_sf,_se=_sg;}return _se;}),_sh=[0,_rY,_s0,_sc],_si=new T(function(){return B(unCStr("change"));}),_sj=new T(function(){return B(_aP(_si,_b));}),_sk=new T(function(){var _sl=E(_sj);if(!_sl[0]){var _sm=B(_N(_si,_rx));}else{var _sn=_sl[2];switch(E(E(_sl[1])[1])){case 104:var _so=E(_sn);if(!_so[0]){var _sp=B(_N(_si,_rx));}else{switch(E(E(_so[1])[1])){case 99:var _sq=B(_N(_si,_ry));break;case 115:var _sq=B(_N(_si,_ry));break;default:var _sq=B(_N(_si,_rx));}var _sr=_sq,_sp=_sr;}var _ss=_sp;break;case 115:var _st=E(_sn);if(!_st[0]){var _su=B(_N(_si,_rx));}else{var _su=E(E(_st[1])[1])==115?B(_N(_si,_ry)):B(_N(_si,_rx));}var _ss=_su;break;case 120:var _ss=B(_N(_si,_ry));break;case 121:var _ss=B(_N(B(_aP(_sn,_b)),_rz));break;default:var _ss=B(_N(_si,_rx));}var _sv=_ss,_sm=_sv;}return _sm;}),_sw=new T(function(){var _sx=E(_sj);if(!_sx[0]){var _sy=B(_N(_si,_rO));}else{switch(E(E(_sx[1])[1])){case 101:var _sz=B(_N(_si,_rQ));break;case 121:var _sz=B(_N(B(_aP(_sx[2],_b)),_rR));break;default:var _sz=B(_N(_si,_rO));}var _sA=_sz,_sy=_sA;}return _sy;}),_sB=[0,_si,_sk,_sw],_sC=new T(function(){return B(unCStr("run"));}),_sD=new T(function(){return B(unCStr("runs"));}),_sE=new T(function(){return B(unCStr("ran"));}),_sF=[0,_sC,_sD,_sE],_sG=new T(function(){return B(unCStr("rise"));}),_sH=new T(function(){return B(unCStr("rises"));}),_sI=new T(function(){return B(unCStr("rosen"));}),_sJ=[0,_sG,_sH,_sI],_sK=[1,_sJ,_b],_sL=[1,_sF,_sK],_sM=[1,_sB,_sL],_sN=[1,_sh,_sM],_sO=[1,_rX,_sN],_sP=new T(function(){return B(unCStr("be"));}),_sQ=new T(function(){return B(unCStr("is"));}),_sR=new T(function(){return B(unCStr("been"));}),_sS=[0,_sP,_sQ,_sR],_sT=[1,_sS,_b],_sU=new T(function(){return B(unCStr("believe"));}),_sV=new T(function(){return B(_aP(_sU,_b));}),_sW=new T(function(){var _sX=E(_sV);if(!_sX[0]){var _sY=B(_N(_sU,_rx));}else{var _sZ=_sX[2];switch(E(E(_sX[1])[1])){case 104:var _t0=E(_sZ);if(!_t0[0]){var _t1=B(_N(_sU,_rx));}else{switch(E(E(_t0[1])[1])){case 99:var _t2=B(_N(_sU,_ry));break;case 115:var _t2=B(_N(_sU,_ry));break;default:var _t2=B(_N(_sU,_rx));}var _t3=_t2,_t1=_t3;}var _t4=_t1;break;case 115:var _t5=E(_sZ);if(!_t5[0]){var _t6=B(_N(_sU,_rx));}else{var _t6=E(E(_t5[1])[1])==115?B(_N(_sU,_ry)):B(_N(_sU,_rx));}var _t4=_t6;break;case 120:var _t4=B(_N(_sU,_ry));break;case 121:var _t4=B(_N(B(_aP(_sZ,_b)),_rz));break;default:var _t4=B(_N(_sU,_rx));}var _t7=_t4,_sY=_t7;}return _sY;}),_t8=new T(function(){var _t9=E(_sV);if(!_t9[0]){var _ta=B(_N(_sU,_rO));}else{switch(E(E(_t9[1])[1])){case 101:var _tb=B(_N(_sU,_rQ));break;case 121:var _tb=B(_N(B(_aP(_t9[2],_b)),_rR));break;default:var _tb=B(_N(_sU,_rO));}var _tc=_tb,_ta=_tc;}return _ta;}),_td=[0,_sU,_sW,_t8],_te=new T(function(){return B(unCStr("assert"));}),_tf=new T(function(){return B(_aP(_te,_b));}),_tg=new T(function(){var _th=E(_tf);if(!_th[0]){var _ti=B(_N(_te,_rx));}else{var _tj=_th[2];switch(E(E(_th[1])[1])){case 104:var _tk=E(_tj);if(!_tk[0]){var _tl=B(_N(_te,_rx));}else{switch(E(E(_tk[1])[1])){case 99:var _tm=B(_N(_te,_ry));break;case 115:var _tm=B(_N(_te,_ry));break;default:var _tm=B(_N(_te,_rx));}var _tn=_tm,_tl=_tn;}var _to=_tl;break;case 115:var _tp=E(_tj);if(!_tp[0]){var _tq=B(_N(_te,_rx));}else{var _tq=E(E(_tp[1])[1])==115?B(_N(_te,_ry)):B(_N(_te,_rx));}var _to=_tq;break;case 120:var _to=B(_N(_te,_ry));break;case 121:var _to=B(_N(B(_aP(_tj,_b)),_rz));break;default:var _to=B(_N(_te,_rx));}var _tr=_to,_ti=_tr;}return _ti;}),_ts=new T(function(){var _tt=E(_tf);if(!_tt[0]){var _tu=B(_N(_te,_rO));}else{switch(E(E(_tt[1])[1])){case 101:var _tv=B(_N(_te,_rQ));break;case 121:var _tv=B(_N(B(_aP(_tt[2],_b)),_rR));break;default:var _tv=B(_N(_te,_rO));}var _tw=_tv,_tu=_tw;}return _tu;}),_tx=[0,_te,_tg,_ts],_ty=[1,_tx,_b],_tz=[1,_td,_ty],_tA=new T(function(){return B(unCStr("try"));}),_tB=new T(function(){return B(_aP(_tA,_b));}),_tC=new T(function(){var _tD=E(_tB);if(!_tD[0]){var _tE=B(_N(_tA,_rx));}else{var _tF=_tD[2];switch(E(E(_tD[1])[1])){case 104:var _tG=E(_tF);if(!_tG[0]){var _tH=B(_N(_tA,_rx));}else{switch(E(E(_tG[1])[1])){case 99:var _tI=B(_N(_tA,_ry));break;case 115:var _tI=B(_N(_tA,_ry));break;default:var _tI=B(_N(_tA,_rx));}var _tJ=_tI,_tH=_tJ;}var _tK=_tH;break;case 115:var _tL=E(_tF);if(!_tL[0]){var _tM=B(_N(_tA,_rx));}else{var _tM=E(E(_tL[1])[1])==115?B(_N(_tA,_ry)):B(_N(_tA,_rx));}var _tK=_tM;break;case 120:var _tK=B(_N(_tA,_ry));break;case 121:var _tK=B(_N(B(_aP(_tF,_b)),_rz));break;default:var _tK=B(_N(_tA,_rx));}var _tN=_tK,_tE=_tN;}return _tE;}),_tO=new T(function(){var _tP=E(_tB);if(!_tP[0]){var _tQ=B(_N(_tA,_rO));}else{switch(E(E(_tP[1])[1])){case 101:var _tR=B(_N(_tA,_rQ));break;case 121:var _tR=B(_N(B(_aP(_tP[2],_b)),_rR));break;default:var _tR=B(_N(_tA,_rO));}var _tS=_tR,_tQ=_tS;}return _tQ;}),_tT=[0,_tA,_tC,_tO],_tU=new T(function(){return B(unCStr("wish"));}),_tV=new T(function(){return B(_aP(_tU,_b));}),_tW=new T(function(){var _tX=E(_tV);if(!_tX[0]){var _tY=B(_N(_tU,_rx));}else{var _tZ=_tX[2];switch(E(E(_tX[1])[1])){case 104:var _u0=E(_tZ);if(!_u0[0]){var _u1=B(_N(_tU,_rx));}else{switch(E(E(_u0[1])[1])){case 99:var _u2=B(_N(_tU,_ry));break;case 115:var _u2=B(_N(_tU,_ry));break;default:var _u2=B(_N(_tU,_rx));}var _u3=_u2,_u1=_u3;}var _u4=_u1;break;case 115:var _u5=E(_tZ);if(!_u5[0]){var _u6=B(_N(_tU,_rx));}else{var _u6=E(E(_u5[1])[1])==115?B(_N(_tU,_ry)):B(_N(_tU,_rx));}var _u4=_u6;break;case 120:var _u4=B(_N(_tU,_ry));break;case 121:var _u4=B(_N(B(_aP(_tZ,_b)),_rz));break;default:var _u4=B(_N(_tU,_rx));}var _u7=_u4,_tY=_u7;}return _tY;}),_u8=new T(function(){var _u9=E(_tV);if(!_u9[0]){var _ua=B(_N(_tU,_rO));}else{switch(E(E(_u9[1])[1])){case 101:var _ub=B(_N(_tU,_rQ));break;case 121:var _ub=B(_N(B(_aP(_u9[2],_b)),_rR));break;default:var _ub=B(_N(_tU,_rO));}var _uc=_ub,_ua=_uc;}return _ua;}),_ud=[0,_tU,_tW,_u8],_ue=[1,_ud,_b],_uf=[1,_tT,_ue],_ug=[4,_,_7p,_gL],_uh=[4,_,_ug,_gK],_ui=new T(function(){return B(unCStr("by"));}),_uj=[1,_ui,_b],_uk=function(_ul){while(1){var _um=(function(_un){var _uo=E(_un);if(!_uo[0]){return [0];}else{var _up=_uo[2],_uq=E(_uo[1]),_ur=_uq[1],_us=function(_ut){var _uu=E(_ut);if(!_uu[0]){return [0];}else{var _uv=E(_uu[1]);return [1,[0,[0,_uh,_ur],_uv[2],_uv[3]],new T(function(){return B(_N(_b,new T(function(){return B(_us(_uu[2]));},1)));})];}};if(!B(_6l(_5H,_ur,_uj))){var _uw=B(_us(_b));if(!_uw[0]){_ul=_up;return null;}else{return [1,_uw[1],new T(function(){return B(_N(_uw[2],new T(function(){return B(_uk(_up));},1)));})];}}else{var _ux=B(_us([1,[0,_j,_uq[2],_uq[3]],_b]));if(!_ux[0]){_ul=_up;return null;}else{return [1,_ux[1],new T(function(){return B(_N(_ux[2],new T(function(){return B(_uk(_up));},1)));})];}}}})(_ul);if(_um!=null){return _um;}}},_uy=function(_uz){var _uA=E(_uz);if(!_uA[0]){return [0];}else{var _uB=E(_uA[1]);return [1,[0,[18,_,_uB[1]],_uB[2],_uB[3]],new T(function(){return B(_N(_b,new T(function(){return B(_uy(_uA[2]));},1)));})];}},_uC=[4,_,_7p,_ph],_uD=function(_uE){var _uF=E(_uE);if(!_uF[0]){return [0];}else{var _uG=E(_uF[1]);return [1,[0,[0,_uC,_uG[1]],_uG[2],_uG[3]],new T(function(){return B(_N(_b,new T(function(){return B(_uD(_uF[2]));},1)));})];}},_uH=[5,_,_7p,_7p],_uI=function(_uJ){var _uK=E(_uJ);if(!_uK[0]){return [0];}else{var _uL=E(_uK[1]);return [1,[0,[0,_uH,_uL[1]],_uL[2],_uL[3]],new T(function(){return B(_N(_b,new T(function(){return B(_uI(_uK[2]));},1)));})];}},_uM=[4,_,_7p,_7x],_uN=function(_uO){var _uP=E(_uO);if(!_uP[0]){return [0];}else{var _uQ=E(_uP[1]);return [1,[0,[0,_uM,_uQ[1]],_uQ[2],_uQ[3]],new T(function(){return B(_N(_b,new T(function(){return B(_uN(_uP[2]));},1)));})];}},_uR=function(_uS){var _uT=E(_uS);if(!_uT[0]){return [0];}else{var _uU=E(_uT[1]);return [1,[0,[0,_7p,_uU[1]],_uU[2],_uU[3]],new T(function(){return B(_N(_b,new T(function(){return B(_uR(_uT[2]));},1)));})];}},_uV=new T(function(){return B(_uk(_b));}),_uW=function(_uX,_uY,_uZ,_v0,_v1){var _v2=function(_v3){while(1){var _v4=(function(_v5){var _v6=E(_v5);if(!_v6[0]){return [0];}else{var _v7=_v6[2],_v8=E(_v6[1]),_v9=function(_va){var _vb=E(_va);if(!_vb[0]){return [0];}else{var _vc=E(_vb[1]);return [1,[0,function(_vd){return new F(function(){return A(_v8[1],[_vd,_vc[1]]);});},_vc[2],_vc[3]],new T(function(){return B(_N(_b,new T(function(){return B(_v9(_vb[2]));},1)));})];}},_ve=B(_v9(B(_uW(_uX,_uY,_uZ,_v8[2],_v8[3]))));if(!_ve[0]){_v3=_v7;return null;}else{return [1,_ve[1],new T(function(){return B(_N(_ve[2],new T(function(){return B(_v2(_v7));},1)));})];}}})(_v3);if(_v4!=null){return _v4;}}},_vf=function(_vg){while(1){var _vh=(function(_vi){var _vj=E(_vi);if(!_vj[0]){return [0];}else{var _vk=_vj[2],_vl=E(_vj[1]),_vm=_vl[2],_vn=_vl[3],_vo=function(_vp){var _vq=E(_vp);if(!_vq[0]){return [0];}else{var _vr=E(_vq[1]);return [1,[0,new T(function(){return B(A(_vr[1],[_vl[1]]));}),_vr[2],_vr[3]],new T(function(){return B(_N(_b,new T(function(){return B(_vo(_vq[2]));},1)));})];}},_vs=B(_vo(B(_N(B(_v2(B(A(_uY,[_uZ,_vm,_vn])))),[1,[0,_9h,_vm,_vn],_b]))));if(!_vs[0]){_vg=_vk;return null;}else{return [1,_vs[1],new T(function(){return B(_N(_vs[2],new T(function(){return B(_vf(_vk));},1)));})];}}})(_vg);if(_vh!=null){return _vh;}}};return new F(function(){return _vf(B(A(_uX,[_uZ,_v0,_v1])));});},_vt=function(_vu,_vv,_vw,_vx){return [1,[0,[0,_vu,_j],_vw,_vx],_b];},_vy=function(_vz){var _vA=E(_vz);if(!_vA[0]){return [0];}else{var _vB=E(_vA[1]);return [1,[0,_j,_vB[2],_vB[3]],new T(function(){return B(_N(_b,new T(function(){return B(_vy(_vA[2]));},1)));})];}},_vC=function(_vD){var _vE=E(_vD);if(!_vE[0]){return [0];}else{var _vF=E(_vE[1]);return [1,[0,_j,_vF[2],_vF[3]],new T(function(){return B(_N(_b,new T(function(){return B(_vC(_vE[2]));},1)));})];}},_vG=new T(function(){return B(unCStr("not"));}),_vH=function(_vI){while(1){var _vJ=(function(_vK){var _vL=E(_vK);if(!_vL[0]){return [0];}else{var _vM=_vL[2],_vN=E(_vL[1]),_vO=_vN[1],_vP=function(_vQ){var _vR=E(_vQ);if(!_vR[0]){return [0];}else{var _vS=E(_vR[1]);return [1,[0,_vO,_vS[2],_vS[3]],new T(function(){return B(_N(_b,new T(function(){return B(_vP(_vR[2]));},1)));})];}},_vT=function(_vU){while(1){var _vV=(function(_vW){var _vX=E(_vW);if(!_vX[0]){return [0];}else{var _vY=_vX[2],_vZ=E(_vX[1]),_w0=B(_vP(B(_gN(_vG,_vZ[2],_vZ[3]))));if(!_w0[0]){_vU=_vY;return null;}else{return [1,_w0[1],new T(function(){return B(_N(_w0[2],new T(function(){return B(_vT(_vY));},1)));})];}}})(_vU);if(_vV!=null){return _vV;}}};if(!B(_6q(_vO,_sP))){var _w1=B(_vT(_b));if(!_w1[0]){_vI=_vM;return null;}else{return [1,_w1[1],new T(function(){return B(_N(_w1[2],new T(function(){return B(_vH(_vM));},1)));})];}}else{var _w2=B(_vT([1,[0,_j,_vN[2],_vN[3]],_b]));if(!_w2[0]){_vI=_vM;return null;}else{return [1,_w2[1],new T(function(){return B(_N(_w2[2],new T(function(){return B(_vH(_vM));},1)));})];}}}})(_vI);if(_vJ!=null){return _vJ;}}},_w3=function(_w4){while(1){var _w5=(function(_w6){var _w7=E(_w6);if(!_w7[0]){return [0];}else{var _w8=_w7[2],_w9=E(_w7[1]),_wa=_w9[1],_wb=function(_wc){var _wd=E(_wc);if(!_wd[0]){return [0];}else{var _we=E(_wd[1]);return [1,[0,_wa,_we[2],_we[3]],new T(function(){return B(_N(_b,new T(function(){return B(_wb(_wd[2]));},1)));})];}};if(!B(_5z(_wa,_sP))){var _wf=B(_wb([1,[0,_j,_w9[2],_w9[3]],_b]));if(!_wf[0]){_w4=_w8;return null;}else{return [1,_wf[1],new T(function(){return B(_N(_wf[2],new T(function(){return B(_w3(_w8));},1)));})];}}else{var _wg=B(_wb(_b));if(!_wg[0]){_w4=_w8;return null;}else{return [1,_wg[1],new T(function(){return B(_N(_wg[2],new T(function(){return B(_w3(_w8));},1)));})];}}}})(_w4);if(_w5!=null){return _w5;}}},_wh=new T(function(){return B(unCStr("will"));}),_wi=new T(function(){return B(unCStr("does"));}),_wj=[2,E(_5y)],_wk=new T(function(){return B(unCStr("has"));}),_wl=function(_wm,_wn,_wo,_wp,_wq){var _wr=function(_ws,_wt,_wu){var _wv=function(_ww){while(1){var _wx=(function(_wy){var _wz=E(_wy);if(!_wz[0]){return [0];}else{var _wA=_wz[2],_wB=E(_wz[1]),_wC=_wB[1],_wD=function(_wE){var _wF=E(_wE);if(!_wF[0]){return E(_h7);}else{var _wG=E(_wF[1]),_wH=_wG[1],_wI=new T(function(){return B(_wD(_wF[2]));}),_wJ=E(_wn);switch(_wJ[0]){case 0:return !B(_6q(_wH,_wC))?E(_wI):function(_wK,_wL,_wM){return [1,[0,_wH,_wL,_wM],new T(function(){return B(A(_wI,[_wK,_wL,_wM]));})];};case 1:return !B(_6q(_wG[3],_wC))?E(_wI):function(_wN,_wO,_wP){return [1,[0,_wH,_wO,_wP],new T(function(){return B(A(_wI,[_wN,_wO,_wP]));})];};case 2:return !E(_wJ[1])?E(_wI):!B(_6q(_wG[2],_wC))?E(_wI):function(_wQ,_wR,_wS){return [1,[0,_wH,_wR,_wS],new T(function(){return B(A(_wI,[_wQ,_wR,_wS]));})];};default:return E(_wI);}}},_wT=B(A(_wD,[_wm,_ws,_wB[2],_wB[3]]));if(!_wT[0]){_ww=_wA;return null;}else{return [1,_wT[1],new T(function(){return B(_N(_wT[2],new T(function(){return B(_wv(_wA));},1)));})];}}})(_ww);if(_wx!=null){return _wx;}}},_wU=E(_wu);return _wU[0]==0?B(_wv(_b)):B(_wv([1,[0,_wU[1],_wt,_wU[2]],_b]));},_wV=E(_wn);switch(_wV[0]){case 2:if(!E(_wV[1])){var _wW=function(_wX){while(1){var _wY=(function(_wZ){var _x0=E(_wZ);if(!_x0[0]){return [0];}else{var _x1=_x0[2],_x2=E(_x0[1]),_x3=B(_w3(B(_x4(_wm,_pj,_wo,_x2[2],_x2[3]))));if(!_x3[0]){_wX=_x1;return null;}else{return [1,_x3[1],new T(function(){return B(_N(_x3[2],new T(function(){return B(_wW(_x1));},1)));})];}}})(_wX);if(_wY!=null){return _wY;}}},_x5=function(_x6){while(1){var _x7=(function(_x8){var _x9=E(_x8);if(!_x9[0]){return [0];}else{var _xa=_x9[2],_xb=E(_x9[1]),_xc=B(_wW(B(_gN(_vG,_xb[2],_xb[3]))));if(!_xc[0]){_x6=_xa;return null;}else{return [1,_xc[1],new T(function(){return B(_N(_xc[2],new T(function(){return B(_x5(_xa));},1)));})];}}})(_x6);if(_x7!=null){return _x7;}}};return new F(function(){return _N(B(_x5(B(_gN(_wi,_wp,_wq)))),new T(function(){return B(_vH(B(_x4(_wm,_wj,_wo,_wp,_wq))));},1));});}else{return new F(function(){return _wr(_wo,_wp,_wq);});}break;case 3:var _xd=function(_xe){while(1){var _xf=(function(_xg){var _xh=E(_xg);if(!_xh[0]){return [0];}else{var _xi=_xh[2],_xj=E(_xh[1]),_xk=B(_x4(_wm,_pj,_wo,_xj[2],_xj[3]));if(!_xk[0]){_xe=_xi;return null;}else{return [1,_xk[1],new T(function(){return B(_N(_xk[2],new T(function(){return B(_xd(_xi));},1)));})];}}})(_xe);if(_xf!=null){return _xf;}}},_xl=function(_xm){while(1){var _xn=(function(_xo){var _xp=E(_xo);if(!_xp[0]){return [0];}else{var _xq=_xp[2],_xr=E(_xp[1]),_xs=_xr[2],_xt=_xr[3];if(!E(_wV[1])){var _xu=B(_xd(B(_vC(B(_gN(_vG,_xs,_xt))))));if(!_xu[0]){_xm=_xq;return null;}else{return [1,_xu[1],new T(function(){return B(_N(_xu[2],new T(function(){return B(_xl(_xq));},1)));})];}}else{var _xv=B(_xd([1,[0,_j,_xs,_xt],_b]));if(!_xv[0]){_xm=_xq;return null;}else{return [1,_xv[1],new T(function(){return B(_N(_xv[2],new T(function(){return B(_xl(_xq));},1)));})];}}}})(_xm);if(_xn!=null){return _xn;}}};return new F(function(){return _xl(B(_gN(_wh,_wp,_wq)));});break;case 4:var _xw=function(_xx){while(1){var _xy=(function(_xz){var _xA=E(_xz);if(!_xA[0]){return [0];}else{var _xB=_xA[2],_xC=E(_xA[1]),_xD=B(_x4(_wm,_gM,_wo,_xC[2],_xC[3]));if(!_xD[0]){_xx=_xB;return null;}else{return [1,_xD[1],new T(function(){return B(_N(_xD[2],new T(function(){return B(_xw(_xB));},1)));})];}}})(_xx);if(_xy!=null){return _xy;}}},_xE=function(_xF){while(1){var _xG=(function(_xH){var _xI=E(_xH);if(!_xI[0]){return [0];}else{var _xJ=_xI[2],_xK=E(_xI[1]),_xL=_xK[2],_xM=_xK[3];if(!E(_wV[1])){var _xN=B(_xw(B(_vy(B(_gN(_vG,_xL,_xM))))));if(!_xN[0]){_xF=_xJ;return null;}else{return [1,_xN[1],new T(function(){return B(_N(_xN[2],new T(function(){return B(_xE(_xJ));},1)));})];}}else{var _xO=B(_xw([1,[0,_j,_xL,_xM],_b]));if(!_xO[0]){_xF=_xJ;return null;}else{return [1,_xO[1],new T(function(){return B(_N(_xO[2],new T(function(){return B(_xE(_xJ));},1)));})];}}}})(_xF);if(_xG!=null){return _xG;}}};return new F(function(){return _xE(B(_gN(_wk,_wp,_wq)));});break;default:return new F(function(){return _wr(_wo,_wp,_wq);});}},_x4=function(_xP,_xQ,_xR,_oQ,_oR){return new F(function(){return _wl(_xP,_xQ,_xR,_oQ,_oR);});},_xS=function(_xT,_xU,_xV,_xW){return new F(function(){return _oJ(B(_nS(_7p,function(_xR,_oQ,_oR){return new F(function(){return _gw(function(_xR,_oQ,_oR){return new F(function(){return _uW(function(_xR,_oQ,_oR){return new F(function(){return _gw(function(_xX,_xY,_xZ){return new F(function(){return _N(B(_uR(B(_x4(_sO,_xT,_xX,_xY,_xZ)))),new T(function(){var _y0=function(_y1){while(1){var _y2=(function(_y3){var _y4=E(_y3);if(!_y4[0]){return [0];}else{var _y5=_y4[2],_y6=E(_y4[1]),_y7=function(_y8){var _y9=E(_y8);if(!_y9[0]){return [0];}else{var _ya=E(_y9[1]);return [1,[0,[5,_7p,_y6[1],_ya[1]],_ya[2],_ya[3]],new T(function(){return B(_N(_b,new T(function(){return B(_y7(_y9[2]));},1)));})];}},_yb=B(_y7(B(A(_r0,[_xX,_y6[2],_y6[3]]))));if(!_yb[0]){_y1=_y5;return null;}else{return [1,_yb[1],new T(function(){return B(_N(_yb[2],new T(function(){return B(_y0(_y5));},1)));})];}}})(_y1);if(_y2!=null){return _y2;}}};return B(_N(B(_y0(B(_yc(_xT,_xX,_xY,_xZ)))),new T(function(){var _yd=function(_ye){while(1){var _yf=(function(_yg){var _yh=E(_yg);if(!_yh[0]){return [0];}else{var _yi=_yh[2],_yj=E(_yh[1]),_yk=function(_yl){var _ym=E(_yl);if(!_ym[0]){return [0];}else{var _yn=E(_ym[1]);return [1,[0,[16,_,_yj[1],_yn[1]],_yn[2],_yn[3]],new T(function(){return B(_N(_b,new T(function(){return B(_yk(_ym[2]));},1)));})];}},_yo=function(_yp){while(1){var _yq=(function(_yr){var _ys=E(_yr);if(!_ys[0]){return [0];}else{var _yt=_ys[2],_yu=E(_ys[1]),_yv=B(_yk(B(_oJ(B(_nS(_7x,function(_xR,_oQ,_oR){return new F(function(){return _gw(_yw,_vt,_xR,_oQ,_oR);});},_xX,_yu[2],_yu[3]))))));if(!_yv[0]){_yp=_yt;return null;}else{return [1,_yv[1],new T(function(){return B(_N(_yv[2],new T(function(){return B(_yo(_yt));},1)));})];}}})(_yp);if(_yq!=null){return _yq;}}},_yx=B(_yo(B(_gN(_jg,_yj[2],_yj[3]))));if(!_yx[0]){_ye=_yi;return null;}else{return [1,_yx[1],new T(function(){return B(_N(_yx[2],new T(function(){return B(_yd(_yi));},1)));})];}}})(_ye);if(_yf!=null){return _yf;}}};return B(_N(B(_yd(B(_uN(B(_x4(_tz,_xT,_xX,_xY,_xZ)))))),new T(function(){var _yy=function(_yz){while(1){var _yA=(function(_yB){var _yC=E(_yB);if(!_yC[0]){return [0];}else{var _yD=_yC[2],_yE=E(_yC[1]),_yF=function(_yG){var _yH=E(_yG);if(!_yH[0]){return [0];}else{var _yI=E(_yH[1]);return [1,[0,[17,_,_yE[1],_yI[1]],_yI[2],_yI[3]],new T(function(){return B(_N(_b,new T(function(){return B(_yF(_yH[2]));},1)));})];}},_yJ=function(_yK){while(1){var _yL=(function(_yM){var _yN=E(_yM);if(!_yN[0]){return [0];}else{var _yO=_yN[2],_yP=E(_yN[1]),_yQ=B(_yF(B(_xS(_pj,_xX,_yP[2],_yP[3]))));if(!_yQ[0]){_yK=_yO;return null;}else{return [1,_yQ[1],new T(function(){return B(_N(_yQ[2],new T(function(){return B(_yJ(_yO));},1)));})];}}})(_yK);if(_yL!=null){return _yL;}}},_yR=B(_yJ(B(_gN(_h6,_yE[2],_yE[3]))));if(!_yR[0]){_yz=_yD;return null;}else{return [1,_yR[1],new T(function(){return B(_N(_yR[2],new T(function(){return B(_yy(_yD));},1)));})];}}})(_yz);if(_yA!=null){return _yA;}}};return B(_N(B(_yy(B(_uI(B(_x4(_uf,_xT,_xX,_xY,_xZ)))))),new T(function(){var _yS=function(_yT){while(1){var _yU=(function(_yV){var _yW=E(_yV);if(!_yW[0]){return [0];}else{var _yX=_yW[2],_yY=E(_yW[1]),_yZ=function(_z0){var _z1=E(_z0);if(!_z1[0]){return [0];}else{var _z2=E(_z1[1]);return [1,[0,[6,_pi,_yY[1],_z2[1]],_z2[2],_z2[3]],new T(function(){return B(_N(_b,new T(function(){return B(_yZ(_z1[2]));},1)));})];}},_z3=B(_yZ(B(_z4(_xX,_yY[2],_yY[3]))));if(!_z3[0]){_yT=_yX;return null;}else{return [1,_z3[1],new T(function(){return B(_N(_z3[2],new T(function(){return B(_yS(_yX));},1)));})];}}})(_yT);if(_yU!=null){return _yU;}}};return B(_N(B(_yS(B(_uD(B(_x4(_sT,_xT,_xX,_xY,_xZ)))))),new T(function(){return B(_N(B(_uy(B(_yc(_xT,_xX,_xY,_xZ)))),new T(function(){var _z5=function(_z6){while(1){var _z7=(function(_z8){var _z9=E(_z8);if(!_z9[0]){return [0];}else{var _za=_z9[2],_zb=E(_z9[1]),_zc=B(_rn(_zb[1],_xX,_zb[2],_zb[3]));if(!_zc[0]){_z6=_za;return null;}else{return [1,_zc[1],new T(function(){return B(_N(_zc[2],new T(function(){return B(_z5(_za));},1)));})];}}})(_z6);if(_z7!=null){return _z7;}}},_zd=function(_ze){while(1){var _zf=(function(_zg){var _zh=E(_zg);if(!_zh[0]){return [0];}else{var _zi=_zh[2],_zj=E(_zh[1]),_zk=function(_zl){var _zm=E(_zl);if(!_zm[0]){return [0];}else{var _zn=E(_zm[1]);return [1,[0,[22,_,_zn[1],_zj[1]],_zn[2],_zn[3]],new T(function(){return B(_N(_b,new T(function(){return B(_zk(_zm[2]));},1)));})];}},_zo=E(_zj[3]);if(!_zo[0]){var _zp=B(_zk(B(_z5(_uV))));if(!_zp[0]){_ze=_zi;return null;}else{return [1,_zp[1],new T(function(){return B(_N(_zp[2],new T(function(){return B(_zd(_zi));},1)));})];}}else{var _zq=B(_zk(B(_z5(B(_uk([1,[0,_zo[1],_zj[2],_zo[2]],_b]))))));if(!_zq[0]){_ze=_zi;return null;}else{return [1,_zq[1],new T(function(){return B(_N(_zq[2],new T(function(){return B(_zd(_zi));},1)));})];}}}})(_ze);if(_zf!=null){return _zf;}}},_zr=function(_zs){while(1){var _zt=(function(_zu){var _zv=E(_zu);if(!_zv[0]){return [0];}else{var _zw=_zv[2],_zx=E(_zv[1]),_zy=B(_zd(B(_yc(_gM,_xX,_zx[2],_zx[3]))));if(!_zy[0]){_zs=_zw;return null;}else{return [1,_zy[1],new T(function(){return B(_N(_zy[2],new T(function(){return B(_zr(_zw));},1)));})];}}})(_zs);if(_zt!=null){return _zt;}}};return B(_zr(B(_x4(_sT,_xT,_xX,_xY,_xZ))));},1)));},1)));},1)));},1)));},1)));},1));});},_re,_xR,_oQ,_oR);});},_px,_xR,_oQ,_oR);});},_vt,_xR,_oQ,_oR);});},_xU,_xV,_xW)));});},_zz=[4,_,_7x,_7x],_zA=new T(function(){return B(unCStr("necessarily"));}),_zB=[1,_zA,_b],_zC=function(_zD){while(1){var _zE=(function(_zF){var _zG=E(_zF);if(!_zG[0]){return [0];}else{var _zH=_zG[2],_zI=E(_zG[1]),_zJ=_zI[1],_zK=function(_zL){var _zM=E(_zL);if(!_zM[0]){return [0];}else{var _zN=E(_zM[1]);return [1,[0,[0,_zz,_zJ],_zN[2],_zN[3]],new T(function(){return B(_N(_b,new T(function(){return B(_zK(_zM[2]));},1)));})];}};if(!B(_6l(_5H,_zJ,_zB))){var _zO=B(_zK(_b));if(!_zO[0]){_zD=_zH;return null;}else{return [1,_zO[1],new T(function(){return B(_N(_zO[2],new T(function(){return B(_zC(_zH));},1)));})];}}else{var _zP=B(_zK([1,[0,_j,_zI[2],_zI[3]],_b]));if(!_zP[0]){_zD=_zH;return null;}else{return [1,_zP[1],new T(function(){return B(_N(_zP[2],new T(function(){return B(_zC(_zH));},1)));})];}}}})(_zD);if(_zE!=null){return _zE;}}},_zQ=new T(function(){return B(_zC(_b));}),_zR=function(_zS,_zT,_zU){var _zV=function(_zW){while(1){var _zX=(function(_zY){var _zZ=E(_zY);if(!_zZ[0]){return [0];}else{var _A0=_zZ[2],_A1=E(_zZ[1]),_A2=function(_A3){var _A4=E(_A3);if(!_A4[0]){return [0];}else{var _A5=E(_A4[1]);return [1,[0,[6,_p8,_A1[1],_A5[1]],_A5[2],_A5[3]],new T(function(){return B(_N(_b,new T(function(){return B(_A2(_A4[2]));},1)));})];}},_A6=B(_A2(B(_oJ(B(_nS(_7x,function(_xR,_oQ,_oR){return new F(function(){return _gw(_yw,_vt,_xR,_oQ,_oR);});},_zS,_A1[2],_A1[3]))))));if(!_A6[0]){_zW=_A0;return null;}else{return [1,_A6[1],new T(function(){return B(_N(_A6[2],new T(function(){return B(_zV(_A0));},1)));})];}}})(_zW);if(_zX!=null){return _zX;}}},_A7=new T(function(){var _A8=function(_A9){while(1){var _Aa=(function(_Ab){var _Ac=E(_Ab);if(!_Ac[0]){return [0];}else{var _Ad=_Ac[2],_Ae=E(_Ac[1]),_Af=_Ae[1],_Ag=_Ae[2],_Ah=_Ae[3],_Ai=function(_Aj){var _Ak=E(_Aj);if(!_Ak[0]){return [0];}else{var _Al=E(_Ak[1]);return [1,[0,[4,_,_Af,_Al[1]],_Al[2],_Al[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Ai(_Ak[2]));},1)));})];}},_Am=B(_N(B(_Ai(B(_xS(_wj,_zS,_Ag,_Ah)))),new T(function(){var _An=function(_Ao){var _Ap=E(_Ao);if(!_Ap[0]){return [0];}else{var _Aq=E(_Ap[1]);return [1,[0,[11,_,_Af,_Aq[1]],_Aq[2],_Aq[3]],new T(function(){return B(_N(_b,new T(function(){return B(_An(_Ap[2]));},1)));})];}};return B(_N(B(_An(B(_xS(_pd,_zS,_Ag,_Ah)))),new T(function(){var _Ar=function(_As){var _At=E(_As);if(!_At[0]){return [0];}else{var _Au=E(_At[1]);return [1,[0,[12,_,_Af,_Au[1]],_Au[2],_Au[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Ar(_At[2]));},1)));})];}};return B(_N(B(_Ar(B(_xS(_pc,_zS,_Ag,_Ah)))),new T(function(){var _Av=function(_Aw){var _Ax=E(_Aw);if(!_Ax[0]){return [0];}else{var _Ay=E(_Ax[1]);return [1,[0,[13,_,_Af,_Ay[1]],_Ay[2],_Ay[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Av(_Ax[2]));},1)));})];}};return B(_N(B(_Av(B(_xS(_pb,_zS,_Ag,_Ah)))),new T(function(){var _Az=function(_AA){var _AB=E(_AA);if(!_AB[0]){return [0];}else{var _AC=E(_AB[1]);return [1,[0,[14,_,_Af,_AC[1]],_AC[2],_AC[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Az(_AB[2]));},1)));})];}};return B(_N(B(_Az(B(_xS(_pa,_zS,_Ag,_Ah)))),new T(function(){var _AD=function(_AE){var _AF=E(_AE);if(!_AF[0]){return [0];}else{var _AG=E(_AF[1]);return [1,[0,[15,_,_Af,_AG[1]],_AG[2],_AG[3]],new T(function(){return B(_N(_b,new T(function(){return B(_AD(_AF[2]));},1)));})];}};return B(_AD(B(_xS(_p9,_zS,_Ag,_Ah))));},1)));},1)));},1)));},1)));},1)));if(!_Am[0]){_A9=_Ad;return null;}else{return [1,_Am[1],new T(function(){return B(_N(_Am[2],new T(function(){return B(_A8(_Ad));},1)));})];}}})(_A9);if(_Aa!=null){return _Aa;}}};return B(_A8(B(A(_pf,[_zS,_zT,_zU]))));}),_AH=E(_zU);if(!_AH[0]){return new F(function(){return _N(B(_zV(_zQ)),_A7);});}else{return new F(function(){return _N(B(_zV(B(_zC([1,[0,_AH[1],_zT,_AH[2]],_b])))),_A7);});}},_yw=function(_xR,_oQ,_oR){return new F(function(){return _uW(_zR,_p4,_xR,_oQ,_oR);});},_AI=function(_AJ,_AK,_AL,_AM,_AN){var _AO=function(_AP){while(1){var _AQ=(function(_AR){var _AS=E(_AR);if(!_AS[0]){return [0];}else{var _AT=_AS[2],_AU=E(_AS[1]),_AV=B(_AI(_AJ,_AU[1],_AL,_AU[2],_AU[3]));if(!_AV[0]){_AP=_AT;return null;}else{return [1,_AV[1],new T(function(){return B(_N(_AV[2],new T(function(){return B(_AO(_AT));},1)));})];}}})(_AP);if(_AQ!=null){return _AQ;}}},_AW=function(_AX){while(1){var _AY=(function(_AZ){var _B0=E(_AZ);if(!_B0[0]){return [0];}else{var _B1=_B0[2],_B2=E(_B0[1]),_B3=_B2[1],_B4=function(_B5){while(1){var _B6=(function(_B7){var _B8=E(_B7);if(!_B8[0]){return [0];}else{var _B9=_B8[2],_Ba=E(_B8[1]),_Bb=_Ba[1],_Bc=E(_B3),_Bd=function(_Be){var _Bf=E(_Be);if(!_Bf[0]){return [0];}else{var _Bg=E(_Bf[1]);return [1,[0,[3,_,_Bc,_AK,_Bb],_Bg[2],_Bg[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Bd(_Bf[2]));},1)));})];}},_Bh=function(_Bi){while(1){var _Bj=(function(_Bk){var _Bl=E(_Bk);if(!_Bl[0]){return [0];}else{var _Bm=_Bl[2],_Bn=E(_Bl[1]),_Bo=B(_Bd(B(_j2(_Bc,_Bn[2],_Bn[3]))));if(!_Bo[0]){_Bi=_Bm;return null;}else{return [1,_Bo[1],new T(function(){return B(_N(_Bo[2],new T(function(){return B(_Bh(_Bm));},1)));})];}}})(_Bi);if(_Bj!=null){return _Bj;}}};if(!B(_iS(_Bc[1],B(_m8(_Bb))))){var _Bp=B(_Bh(_b));if(!_Bp[0]){_B5=_B9;return null;}else{return [1,_Bp[1],new T(function(){return B(_N(_Bp[2],new T(function(){return B(_B4(_B9));},1)));})];}}else{var _Bq=B(_Bh([1,[0,_j,_Ba[2],_Ba[3]],_b]));if(!_Bq[0]){_B5=_B9;return null;}else{return [1,_Bq[1],new T(function(){return B(_N(_Bq[2],new T(function(){return B(_B4(_B9));},1)));})];}}}})(_B5);if(_B6!=null){return _B6;}}},_Br=B(_B4(B(_oJ(B(_nS(_7x,function(_xR,_oQ,_oR){return new F(function(){return _gw(_yw,_vt,_xR,_oQ,_oR);});},new T(function(){return [0,[1,[0,_B3,_AJ],E(_AL)[1]]];}),_B2[2],_B2[3]))))));if(!_Br[0]){_AX=_B1;return null;}else{return [1,_Br[1],new T(function(){return B(_N(_Br[2],new T(function(){return B(_AW(_B1));},1)));})];}}})(_AX);if(_AY!=null){return _AY;}}},_Bs=function(_Bt){while(1){var _Bu=(function(_Bv){var _Bw=E(_Bv);if(!_Bw[0]){return [0];}else{var _Bx=_Bw[2],_By=E(_Bw[1]),_Bz=_By[2],_BA=B(_AW(B(_ml([1,[0,_Bz,_Bz,_By[3]],_b]))));if(!_BA[0]){_Bt=_Bx;return null;}else{return [1,_BA[1],new T(function(){return B(_N(_BA[2],new T(function(){return B(_Bs(_Bx));},1)));})];}}})(_Bt);if(_Bu!=null){return _Bu;}}},_BB=function(_BC){while(1){var _BD=(function(_BE){var _BF=E(_BE);if(!_BF[0]){return [0];}else{var _BG=_BF[2],_BH=E(_BF[1]),_BI=B(_Bs(B(_gN(_jg,_BH[2],_BH[3]))));if(!_BI[0]){_BC=_BG;return null;}else{return [1,_BI[1],new T(function(){return B(_N(_BI[2],new T(function(){return B(_BB(_BG));},1)));})];}}})(_BC);if(_BD!=null){return _BD;}}};return new F(function(){return _N(B(_AO(B(_BB(B(_gN(_jf,_AM,_AN)))))),[1,[0,_AK,_AM,_AN],_b]);});},_BJ=function(_BK,_BL,_BM,_BN,_BO){var _BP=function(_BQ){var _BR=E(_BQ);if(!_BR[0]){return [0];}else{var _BS=E(_BR[1]);return [1,[0,[0,_BS[1],_BL],_BS[2],_BS[3]],new T(function(){return B(_N(_b,new T(function(){return B(_BP(_BR[2]));},1)));})];}};return new F(function(){return _BP(B(_AI(_BL,_BK,_BM,_BN,_BO)));});},_BT=function(_BU,_BV,_BW,_BX){var _BY=E(_BU);return new F(function(){return _BJ(_BY[1],_BY[2],_BV,_BW,_BX);});},_BZ=function(_xR,_oQ,_oR){return new F(function(){return _gw(_iM,_BT,_xR,_oQ,_oR);});},_C0=[4,_,_7x,_7p],_C1=function(_oQ,_oR){return [9,_C0,_oQ,_oR];},_C2=function(_C3){var _C4=E(_C3);if(!_C4[0]){return [0];}else{var _C5=E(_C4[1]);return [1,[0,_C1,_C5[2],_C5[3]],new T(function(){return B(_N(_b,new T(function(){return B(_C2(_C4[2]));},1)));})];}},_C6=function(_C7,_C8){return new F(function(){return _C2(B(_gN(_oN,_C7,_C8)));});},_C9=function(_Ca,_Cb,_Cc){return new F(function(){return _C6(_Cb,_Cc);});},_Cd=[4,_,_gK,_el],_Ce=new T(function(){return B(unCStr("the"));}),_Cf=new T(function(){return B(unCStr("every"));}),_Cg=new T(function(){return B(unCStr("no"));}),_Ch=[1,_Cg,_b],_Ci=[1,_Cf,_Ch],_Cj=[1,_Ce,_Ci],_Ck=new T(function(){return B(unCStr("an"));}),_Cl=[1,_Ck,_Cj],_Cm=[0,97],_Cn=[1,_Cm,_b],_Co=[1,_Cn,_Cl],_Cp=function(_Cq){while(1){var _Cr=(function(_Cs){var _Ct=E(_Cs);if(!_Ct[0]){return [0];}else{var _Cu=_Ct[2],_Cv=E(_Ct[1]),_Cw=_Cv[1],_Cx=function(_Cy){var _Cz=E(_Cy);if(!_Cz[0]){return [0];}else{var _CA=E(_Cz[1]);return [1,[0,[0,_Cd,_Cw],_CA[2],_CA[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Cx(_Cz[2]));},1)));})];}};if(!B(_6l(_5H,_Cw,_Co))){var _CB=B(_Cx(_b));if(!_CB[0]){_Cq=_Cu;return null;}else{return [1,_CB[1],new T(function(){return B(_N(_CB[2],new T(function(){return B(_Cp(_Cu));},1)));})];}}else{var _CC=B(_Cx([1,[0,_j,_Cv[2],_Cv[3]],_b]));if(!_CC[0]){_Cq=_Cu;return null;}else{return [1,_CC[1],new T(function(){return B(_N(_CC[2],new T(function(){return B(_Cp(_Cu));},1)));})];}}}})(_Cq);if(_Cr!=null){return _Cr;}}},_CD=function(_CE){var _CF=E(_CE);if(!_CF[0]){return [0];}else{var _CG=E(_CF[1]);return [1,[0,E(_CG[1])[2],_CG[2],_CG[3]],new T(function(){return B(_N(_b,new T(function(){return B(_CD(_CF[2]));},1)));})];}},_CH=function(_CI){var _CJ=E(_CI);if(!_CJ[0]){return [0];}else{var _CK=E(_CJ[1]);return [1,[0,new T(function(){return E(E(_CK[1])[1]);}),_CK[2],_CK[3]],new T(function(){return B(_N(_b,new T(function(){return B(_CH(_CJ[2]));},1)));})];}},_CL=new T(function(){return B(unCStr("Pattern match failure in do expression at ../src/Parser.hs:243:8-16"));}),_CM=new T(function(){return B(err(_CL));}),_CN=[0,_Cd,_Cn],_CO=function(_CP){var _CQ=E(_CP);if(!_CQ[0]){return [0];}else{var _CR=E(_CQ[1]),_CS=E(_CR[1]);return _CS[0]==0?[1,[0,new T(function(){return !B(_6q(_CS[2],_Ck))?E(_CS):E(_CN);}),_CR[2],_CR[3]],new T(function(){return B(_N(_b,new T(function(){return B(_CO(_CQ[2]));},1)));})]:E(_CM);}},_CT=new T(function(){return B(unCStr("ninety"));}),_CU=[0,_CT,_hx],_CV=[1,_CU,_b],_CW=new T(function(){return B(unCStr("bill"));}),_CX=[0,_CW,_hn],_CY=[1,_CX,_CV],_CZ=new T(function(){return B(unCStr("mary"));}),_D0=[0,_CZ,_hs],_D1=[1,_D0,_CY],_D2=new T(function(){return B(unCStr("john"));}),_D3=[0,_D2,_hn],_D4=[1,_D3,_D1],_D5=function(_D6){while(1){var _D7=(function(_D8){var _D9=E(_D8);if(!_D9[0]){return [0];}else{var _Da=_D9[2],_Db=E(_D9[1]),_Dc=_Db[1],_Dd=B(_is(_5H,_Dc,_D4));if(!_Dd[0]){_D6=_Da;return null;}else{return [1,[0,[0,[0,_gK,_Dc],_Dd[1]],_Db[2],_Db[3]],new T(function(){return B(_N(_b,new T(function(){return B(_D5(_Da));},1)));})];}}})(_D6);if(_D7!=null){return _D7;}}},_De=function(_Df){while(1){var _Dg=(function(_Dh){var _Di=E(_Dh);if(!_Di[0]){return [0];}else{var _Dj=_Di[2],_Dk=E(_Di[1]),_Dl=E(_Dk[1]),_Dm=B(_md(_Dl[1],_Dl[2],_Dk[3]));if(!_Dm[0]){_Df=_Dj;return null;}else{return [1,_Dm[1],new T(function(){return B(_N(_Dm[2],new T(function(){return B(_De(_Dj));},1)));})];}}})(_Df);if(_Dg!=null){return _Dg;}}},_Dn=function(_Do){var _Dp=E(_Do);if(!_Dp[0]){return [0];}else{var _Dq=E(_Dp[1]);return [1,[0,E(_Dq[1])[2],_Dq[2],_Dq[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Dn(_Dp[2]));},1)));})];}},_Dr=function(_Ds){var _Dt=E(_Ds);if(!_Dt[0]){return [0];}else{var _Du=E(_Dt[1]),_Dv=_Du[2],_Dw=_Du[3],_Dx=E(_Du[1]),_Dy=_Dx[1];return [1,[0,_Dy,_Dv,_Dw],new T(function(){var _Dz=function(_DA){while(1){var _DB=(function(_DC){var _DD=E(_DC);if(!_DD[0]){return [0];}else{var _DE=_DD[2],_DF=E(_DD[1]),_DG=_DF[1],_DH=_DF[2],_DI=function(_DJ){var _DK=E(_DJ);if(!_DK[0]){return [0];}else{var _DL=E(_DK[1]);return [1,[0,[1,_,_DG],_DL[2],_DL[3]],new T(function(){return B(_N(_b,new T(function(){return B(_DI(_DK[2]));},1)));})];}},_DM=function(_DN){while(1){var _DO=(function(_DP){var _DQ=E(_DP);if(!_DQ[0]){return [0];}else{var _DR=_DQ[2],_DS=E(_DQ[1]),_DT=B(_DI(B(_mu([1,[0,_DG,_Dx[2],_Dy,new T(function(){return B(_m8(_Dy));})],_DS[1]],_DS[2],_DS[3]))));if(!_DT[0]){_DN=_DR;return null;}else{return [1,_DT[1],new T(function(){return B(_N(_DT[2],new T(function(){return B(_DM(_DR));},1)));})];}}})(_DN);if(_DO!=null){return _DO;}}},_DU=B(_DM(B(_Dn([1,[0,_DH,_DH,_DF[3]],_b]))));if(!_DU[0]){_DA=_DE;return null;}else{return [1,_DU[1],new T(function(){return B(_N(_DU[2],new T(function(){return B(_Dz(_DE));},1)));})];}}})(_DA);if(_DB!=null){return _DB;}}};return B(_N(B(_Dz(B(_De([1,[0,_Dv,_Dv,_Dw],_b])))),new T(function(){return B(_Dr(_Dt[2]));},1)));})];}},_DV=new T(function(){return B(_Cp(_b));}),_DW=new T(function(){return B(_D5(_b));}),_pg=function(_DX){return function(_5q,_DY,_DZ){return new F(function(){return _uW(function(_E0,_E1,_E2){var _E3=new T(function(){var _E4=function(_E5){while(1){var _E6=(function(_E7){var _E8=E(_E7);if(!_E8[0]){return [0];}else{var _E9=_E8[2],_Ea=E(_E8[1]),_Eb=_Ea[2],_Ec=function(_Ed,_Ee,_Ef){var _Eg=[1,_,_Ed];switch(E(_Ea[1])){case 0:return E(_Ee)==0?function(_Eh,_Ei,_Ej){return [1,[0,_Eg,_Ei,_Ej],new T(function(){return B(A(_Ef,[_Eh,_Ei,_Ej]));})];}:E(_Ef);case 1:return E(_Ee)==1?function(_Ek,_El,_Em){return [1,[0,_Eg,_El,_Em],new T(function(){return B(A(_Ef,[_Ek,_El,_Em]));})];}:E(_Ef);default:return E(_Ee)==2?function(_En,_Eo,_Ep){return [1,[0,_Eg,_Eo,_Ep],new T(function(){return B(A(_Ef,[_En,_Eo,_Ep]));})];}:E(_Ef);}},_Eq=function(_Er){var _Es=E(_Er);if(!_Es[0]){return E(_h7);}else{var _Et=E(_Es[1]);return new F(function(){return _Ec(_Et[1],_Et[2],new T(function(){return B(_Eq(_Es[2]));}));});}},_Eu=function(_Ev){while(1){var _Ew=(function(_Ex){var _Ey=E(_Ex);if(!_Ey[0]){return [0];}else{var _Ez=_Ey[2],_EA=E(_Ey[1]),_EB=function(_EC){while(1){var _ED=(function(_EE){var _EF=E(_EE);if(!_EF[0]){return [0];}else{var _EG=_EF[2],_EH=E(_EF[1]),_EI=function(_EJ){var _EK=E(_EJ);if(!_EK[0]){return E(new T(function(){return B(_Eq(_EH[1]));}));}else{var _EL=E(_EK[1]);return new F(function(){return _Ec(_EL[1],_EL[2],new T(function(){return B(_EI(_EK[2]));}));});}},_EM=B(A(_EI,[_EA[1],_E0,_EH[2],_EH[3]]));if(!_EM[0]){_EC=_EG;return null;}else{return [1,_EM[1],new T(function(){return B(_N(_EM[2],new T(function(){return B(_EB(_EG));},1)));})];}}})(_EC);if(_ED!=null){return _ED;}}},_EN=B(_EB(B(_CH([1,[0,_E0,_EA[2],_EA[3]],_b]))));if(!_EN[0]){_Ev=_Ez;return null;}else{return [1,_EN[1],new T(function(){return B(_N(_EN[2],new T(function(){return B(_Eu(_Ez));},1)));})];}}})(_Ev);if(_Ew!=null){return _Ew;}}},_EO=B(_Eu(B(_CD([1,[0,_Eb,_Eb,_Ea[3]],_b]))));if(!_EO[0]){_E5=_E9;return null;}else{return [1,_EO[1],new T(function(){return B(_N(_EO[2],new T(function(){return B(_E4(_E9));},1)));})];}}})(_E5);if(_E6!=null){return _E6;}}};return B(_E4(B(_N(B(A(new T(function(){return !B(_6l(_hh,_hj,_DX))?E(_h7):E(_i0);}),[_E0,_E1,_E2])),new T(function(){return B(A(new T(function(){return !B(_6l(_hh,_hi,_DX))?E(_h7):E(_hF);}),[_E0,_E1,_E2]));},1)))));},1),_EP=new T(function(){var _EQ=function(_ER){while(1){var _ES=(function(_ET){var _EU=E(_ET);if(!_EU[0]){return [0];}else{var _EV=_EU[2],_EW=E(_EU[1]),_EX=function(_EY){var _EZ=E(_EY);if(!_EZ[0]){return [0];}else{var _F0=E(_EZ[1]),_F1=E(_F0[1]);return [1,[0,[0,[2,_,_EW[1],_F1[1]],_F1[2]],_F0[2],_F0[3]],new T(function(){return B(_N(_b,new T(function(){return B(_EX(_EZ[2]));},1)));})];}},_F2=B(_EX(B(_nS(_el,_BZ,_E0,_EW[2],_EW[3]))));if(!_F2[0]){_ER=_EV;return null;}else{return [1,_F2[1],new T(function(){return B(_N(_F2[2],new T(function(){return B(_EQ(_EV));},1)));})];}}})(_ER);if(_ES!=null){return _ES;}}},_F3=E(_E2);if(!_F3[0]){var _F4=B(_EQ(B(_CO(_DV))));}else{var _F4=B(_EQ(B(_CO(B(_Cp([1,[0,_F3[1],_E1,_F3[2]],_b]))))));}return _F4;},1),_F5=E(_E2);if(!_F5[0]){return new F(function(){return _N(B(_Dr(B(_N(_DW,_EP)))),_E3);});}else{return new F(function(){return _N(B(_Dr(B(_N(B(_D5([1,[0,_F5[1],_E1,_F5[2]],_b])),_EP)))),_E3);});}},_C9,_5q,_DY,_DZ);});};},_F6=[1,_hi,_b],_r0=new T(function(){return B(_pg(_F6));}),_F7=new T(function(){return B(unCStr("find"));}),_F8=new T(function(){return B(unCStr("finds"));}),_F9=new T(function(){return B(unCStr("found"));}),_Fa=[0,_F7,_F8,_F9],_Fb=new T(function(){return B(unCStr("lose"));}),_Fc=new T(function(){return B(unCStr("loses"));}),_Fd=new T(function(){return B(unCStr("lost"));}),_Fe=[0,_Fb,_Fc,_Fd],_Ff=new T(function(){return B(unCStr("eat"));}),_Fg=new T(function(){return B(unCStr("eats"));}),_Fh=new T(function(){return B(unCStr("eaten"));}),_Fi=[0,_Ff,_Fg,_Fh],_Fj=new T(function(){return B(unCStr("seek"));}),_Fk=new T(function(){return B(unCStr("seeks"));}),_Fl=new T(function(){return B(unCStr("sought"));}),_Fm=[0,_Fj,_Fk,_Fl],_Fn=new T(function(){return B(unCStr("love"));}),_Fo=new T(function(){return B(_aP(_Fn,_b));}),_Fp=new T(function(){var _Fq=E(_Fo);if(!_Fq[0]){var _Fr=B(_N(_Fn,_rx));}else{var _Fs=_Fq[2];switch(E(E(_Fq[1])[1])){case 104:var _Ft=E(_Fs);if(!_Ft[0]){var _Fu=B(_N(_Fn,_rx));}else{switch(E(E(_Ft[1])[1])){case 99:var _Fv=B(_N(_Fn,_ry));break;case 115:var _Fv=B(_N(_Fn,_ry));break;default:var _Fv=B(_N(_Fn,_rx));}var _Fw=_Fv,_Fu=_Fw;}var _Fx=_Fu;break;case 115:var _Fy=E(_Fs);if(!_Fy[0]){var _Fz=B(_N(_Fn,_rx));}else{var _Fz=E(E(_Fy[1])[1])==115?B(_N(_Fn,_ry)):B(_N(_Fn,_rx));}var _Fx=_Fz;break;case 120:var _Fx=B(_N(_Fn,_ry));break;case 121:var _Fx=B(_N(B(_aP(_Fs,_b)),_rz));break;default:var _Fx=B(_N(_Fn,_rx));}var _FA=_Fx,_Fr=_FA;}return _Fr;}),_FB=new T(function(){var _FC=E(_Fo);if(!_FC[0]){var _FD=B(_N(_Fn,_rO));}else{switch(E(E(_FC[1])[1])){case 101:var _FE=B(_N(_Fn,_rQ));break;case 121:var _FE=B(_N(B(_aP(_FC[2],_b)),_rR));break;default:var _FE=B(_N(_Fn,_rO));}var _FF=_FE,_FD=_FF;}return _FD;}),_FG=[0,_Fn,_Fp,_FB],_FH=new T(function(){return B(unCStr("date"));}),_FI=new T(function(){return B(_aP(_FH,_b));}),_FJ=new T(function(){var _FK=E(_FI);if(!_FK[0]){var _FL=B(_N(_FH,_rx));}else{var _FM=_FK[2];switch(E(E(_FK[1])[1])){case 104:var _FN=E(_FM);if(!_FN[0]){var _FO=B(_N(_FH,_rx));}else{switch(E(E(_FN[1])[1])){case 99:var _FP=B(_N(_FH,_ry));break;case 115:var _FP=B(_N(_FH,_ry));break;default:var _FP=B(_N(_FH,_rx));}var _FQ=_FP,_FO=_FQ;}var _FR=_FO;break;case 115:var _FS=E(_FM);if(!_FS[0]){var _FT=B(_N(_FH,_rx));}else{var _FT=E(E(_FS[1])[1])==115?B(_N(_FH,_ry)):B(_N(_FH,_rx));}var _FR=_FT;break;case 120:var _FR=B(_N(_FH,_ry));break;case 121:var _FR=B(_N(B(_aP(_FM,_b)),_rz));break;default:var _FR=B(_N(_FH,_rx));}var _FU=_FR,_FL=_FU;}return _FL;}),_FV=new T(function(){var _FW=E(_FI);if(!_FW[0]){var _FX=B(_N(_FH,_rO));}else{switch(E(E(_FW[1])[1])){case 101:var _FY=B(_N(_FH,_rQ));break;case 121:var _FY=B(_N(B(_aP(_FW[2],_b)),_rR));break;default:var _FY=B(_N(_FH,_rO));}var _FZ=_FY,_FX=_FZ;}return _FX;}),_G0=[0,_FH,_FJ,_FV],_G1=[1,_G0,_sT],_G2=[1,_FG,_G1],_G3=[1,_Fm,_G2],_G4=[1,_Fi,_G3],_G5=[1,_Fe,_G4],_G6=[1,_Fa,_G5],_G7=function(_G8){var _G9=E(_G8);if(!_G9[0]){return [0];}else{var _Ga=E(_G9[1]);return [1,[0,[0,_gL,_Ga[1]],_Ga[2],_Ga[3]],new T(function(){return B(_N(_b,new T(function(){return B(_G7(_G9[2]));},1)));})];}},_yc=function(_Gb,_Gc,_Gd,_Ge){return new F(function(){return _N(B(_G7(B(_x4(_G6,_Gb,_Gc,_Gd,_Ge)))),new T(function(){var _Gf=function(_Gg){while(1){var _Gh=(function(_Gi){var _Gj=E(_Gi);if(!_Gj[0]){return [0];}else{var _Gk=_Gj[2],_Gl=E(_Gj[1]),_Gm=function(_Gn){var _Go=E(_Gn);if(!_Go[0]){return [0];}else{var _Gp=E(_Go[1]);return [1,[0,[19,_,_Gl[1],_Gp[1]],_Gp[2],_Gp[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Gm(_Go[2]));},1)));})];}},_Gq=function(_Gr){while(1){var _Gs=(function(_Gt){var _Gu=E(_Gt);if(!_Gu[0]){return [0];}else{var _Gv=_Gu[2],_Gw=E(_Gu[1]),_Gx=B(_Gm(B(A(_r0,[_Gc,_Gw[2],_Gw[3]]))));if(!_Gx[0]){_Gr=_Gv;return null;}else{return [1,_Gx[1],new T(function(){return B(_N(_Gx[2],new T(function(){return B(_Gq(_Gv));},1)));})];}}})(_Gr);if(_Gs!=null){return _Gs;}}},_Gy=B(_Gq(B(_gN(_h6,_Gl[2],_Gl[3]))));if(!_Gy[0]){_Gg=_Gk;return null;}else{return [1,_Gy[1],new T(function(){return B(_N(_Gy[2],new T(function(){return B(_Gf(_Gk));},1)));})];}}})(_Gg);if(_Gh!=null){return _Gh;}}};return B(_N(B(_Gf(_b)),new T(function(){var _Gz=function(_GA){while(1){var _GB=(function(_GC){var _GD=E(_GC);if(!_GD[0]){return [0];}else{var _GE=_GD[2],_GF=E(_GD[1]),_GG=function(_GH){var _GI=E(_GH);if(!_GI[0]){return [0];}else{var _GJ=E(_GI[1]);return [1,[0,[20,_,_GF[1],_GJ[1]],_GJ[2],_GJ[3]],new T(function(){return B(_N(_b,new T(function(){return B(_GG(_GI[2]));},1)));})];}},_GK=B(_GG(B(A(_r0,[_Gc,_GF[2],_GF[3]]))));if(!_GK[0]){_GA=_GE;return null;}else{return [1,_GK[1],new T(function(){return B(_N(_GK[2],new T(function(){return B(_Gz(_GE));},1)));})];}}})(_GA);if(_GB!=null){return _GB;}}};return B(_Gz(_b));},1)));},1));});},_GL=function(_GM){var _GN=E(_GM);if(!_GN[0]){return [0];}else{var _GO=E(_GN[1]);return [1,[0,[24,_,_GO[1]],_GO[2],_GO[3]],new T(function(){return B(_N(_b,new T(function(){return B(_GL(_GN[2]));},1)));})];}},_GP=new T(function(){return B(unCStr("asleep"));}),_GQ=[1,_GP,_b],_GR=function(_GS){while(1){var _GT=(function(_GU){var _GV=E(_GU);if(!_GV[0]){return [0];}else{var _GW=_GV[2],_GX=E(_GV[1]),_GY=_GX[1],_GZ=function(_H0){var _H1=E(_H0);if(!_H1[0]){return [0];}else{var _H2=E(_H1[1]);return [1,[0,[0,_ph,_GY],_H2[2],_H2[3]],new T(function(){return B(_N(_b,new T(function(){return B(_GZ(_H1[2]));},1)));})];}};if(!B(_6l(_5H,_GY,_GQ))){var _H3=B(_GZ(_b));if(!_H3[0]){_GS=_GW;return null;}else{return [1,_H3[1],new T(function(){return B(_N(_H3[2],new T(function(){return B(_GR(_GW));},1)));})];}}else{var _H4=B(_GZ([1,[0,_j,_GX[2],_GX[3]],_b]));if(!_H4[0]){_GS=_GW;return null;}else{return [1,_H4[1],new T(function(){return B(_N(_H4[2],new T(function(){return B(_GR(_GW));},1)));})];}}}})(_GS);if(_GT!=null){return _GT;}}},_H5=new T(function(){return B(_GR(_b));}),_z4=function(_H6,_H7,_H8){var _H9=new T(function(){return B(_GL(B(_yc(_gM,_H6,_H7,_H8))));},1),_Ha=E(_H8);if(!_Ha[0]){return new F(function(){return _N(_H5,_H9);});}else{return new F(function(){return _N(B(_GR([1,[0,_Ha[1],_H7,_Ha[2]],_b])),_H9);});}},_Hb=[1,_hj,_F6],_Hc=new T(function(){return B(_pg(_Hb));}),_Hd=[4,_,_gL,_gK],_He=function(_Hf){var _Hg=E(_Hf);if(!_Hg[0]){return [0];}else{var _Hh=E(_Hg[1]);return [1,[0,[0,_uC,_Hh[1]],_Hh[2],_Hh[3]],new T(function(){return B(_N(_b,new T(function(){return B(_He(_Hg[2]));},1)));})];}},_Hi=function(_Hj){var _Hk=E(_Hj);if(!_Hk[0]){return [0];}else{var _Hl=E(_Hk[1]);return [1,[0,[0,_uM,_Hl[1]],_Hl[2],_Hl[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Hi(_Hk[2]));},1)));})];}},_Hm=function(_Hn){var _Ho=E(_Hn);if(!_Ho[0]){return [0];}else{var _Hp=E(_Ho[1]);return [1,[0,[0,_uH,_Hp[1]],_Hp[2],_Hp[3]],new T(function(){return B(_N(_b,new T(function(){return B(_Hm(_Ho[2]));},1)));})];}},_Hq=[0,_b],_Hr=[0,0],_Hs=[0,_Hr,_b],_Ht=function(_oR){return [0,_7x,_oR];},_Hu=function(_oR){return [0,_el,_oR];},_Hv=function(_oR){return [0,_ph,_oR];},_Hw=function(_oR){return [0,_zz,_oR];},_Hx=function(_oR){return [0,_gK,_oR];},_Hy=function(_oR){return [0,_q1,_oR];},_Hz=function(_oR){return [0,_ug,_oR];},_HA=function(_oR){return [0,_Cd,_oR];},_HB=function(_oR){return [0,_q2,_oR];},_HC=function(_oR){return [0,_uh,_oR];},_HD=function(_HE){return [2,E(E(_HE))];},_HF=function(_HG){return [3,E(E(_HG))];},_HH=function(_HI){return [4,E(E(_HI))];},_HJ=[1,_HH,_b],_HK=[1,_HF,_HJ],_HL=[1,_HD,_HK],_HM=[1,_gM,_b],_HN=[1,_pj,_HM],_HO=function(_oR){return [0,_uC,_oR];},_HP=function(_HQ){while(1){var _HR=(function(_HS){var _HT=E(_HS);if(!_HT[0]){return [0];}else{var _HU=_HT[2],_HV=E(_HT[1]);if(!E(E(_HV[2])[2])[0]){if(!E(_HV[3])[0]){return [1,_HV[1],new T(function(){return B(_HP(_HU));})];}else{_HQ=_HU;return null;}}else{_HQ=_HU;return null;}}})(_HQ);if(_HR!=null){return _HR;}}},_HW=function(_HX){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_2R(9,_HX,_b));}))));});},_HY=function(_HZ){var _I0=u_towlower(_HZ),_I1=_I0;return _I1>>>0>1114111?B(_HW(_I1)):_I1;},_I2=function(_I3){while(1){var _I4=(function(_I5){var _I6=E(_I5);if(!_I6[0]){return [0];}else{var _I7=_I6[2],_I8=E(E(_I6[1])[1]);if(_I8==46){_I3=_I7;return null;}else{return [1,new T(function(){return [0,B(_HY(_I8))];}),new T(function(){return B(_I2(_I7));})];}}})(_I3);if(_I4!=null){return _I4;}}},_I9=[1,_vG,_b],_Ia=[1,_wi,_I9],_Ib=new T(function(){return B(unCStr("doesn\'t"));}),_Ic=[1,_wh,_I9],_Id=new T(function(){return B(unCStr("won\'t"));}),_Ie=[1,_sQ,_I9],_If=new T(function(){return B(unCStr("isn\'t"));}),_Ig=[1,_wk,_I9],_Ih=new T(function(){return B(unCStr("hasn\'t"));}),_Ii=function(_Ij){var _Ik=E(_Ij);if(!_Ik[0]){return [0];}else{var _Il=_Ik[1],_Im=_Ik[2];if(!B(_6q(_Il,_Ib))){if(!B(_6q(_Il,_Ih))){if(!B(_6q(_Il,_If))){if(!B(_6q(_Il,_Id))){return [1,_Il,new T(function(){return B(_Ii(_Im));})];}else{return new F(function(){return _N(_Ic,new T(function(){return B(_Ii(_Im));},1));});}}else{return new F(function(){return _N(_Ie,new T(function(){return B(_Ii(_Im));},1));});}}else{return new F(function(){return _N(_Ig,new T(function(){return B(_Ii(_Im));},1));});}}else{return new F(function(){return _N(_Ia,new T(function(){return B(_Ii(_Im));},1));});}}},_In=function(_Io,_Ip){var _Iq=E(_Ip);if(!_Iq[0]){return [0,_b,_b];}else{var _Ir=_Iq[1];if(!B(A(_Io,[_Ir]))){var _Is=new T(function(){var _It=B(_In(_Io,_Iq[2]));return [0,_It[1],_It[2]];});return [0,[1,_Ir,new T(function(){return E(E(_Is)[1]);})],new T(function(){return E(E(_Is)[2]);})];}else{return [0,_b,_Iq];}}},_Iu=function(_Iv,_Iw){while(1){var _Ix=E(_Iw);if(!_Ix[0]){return [0];}else{if(!B(A(_Iv,[_Ix[1]]))){return E(_Ix);}else{_Iw=_Ix[2];continue;}}}},_Iy=function(_Iz){var _IA=E(_Iz);switch(_IA){case 9:return true;case 10:return true;case 11:return true;case 12:return true;case 13:return true;case 32:return true;case 160:return true;default:var _IB=u_iswspace(_IA),_IC=_IB;return E(_IC)==0?false:true;}},_ID=function(_IE){return new F(function(){return _Iy(E(_IE)[1]);});},_IF=function(_IG){var _IH=B(_Iu(_ID,_IG));if(!_IH[0]){return [0];}else{var _II=new T(function(){var _IJ=B(_In(_ID,_IH));return [0,_IJ[1],_IJ[2]];});return [1,new T(function(){return E(E(_II)[1]);}),new T(function(){return B(_IF(E(_II)[2]));})];}},_IK=function(_IL){return new F(function(){return _1L([1,new T(function(){var _IM=function(_IN){var _IO=E(_IN);if(!_IO[0]){return [0];}else{var _IP=E(_IO[1]),_IQ=E(_IP[1]);return _IQ[0]==0?[1,[0,new T(function(){return !B(_6q(_IQ[2],_Ck))?E(_IQ):E(_CN);}),_IP[2],_IP[3]],new T(function(){return B(_N(_b,new T(function(){return B(_IM(_IO[2]));},1)));})]:E(_CM);}},_IR=B(_Ii(B(_IF(B(_I2(_IL))))));if(!_IR[0]){var _IS=B(_af(_HA,B(_HP(B(_IM(_DV))))));}else{var _IS=B(_af(_HA,B(_HP(B(_IM(B(_Cp([1,[0,_IR[1],_Hs,_IR[2]],_b]))))))));}var _IT=_IS,_IU=_IT;return _IU;}),[1,new T(function(){var _IV=function(_IW){var _IX=E(_IW);if(!_IX[0]){return [0];}else{var _IY=E(_IX[1]);return [1,[0,new T(function(){return E(E(_IY[1])[1]);}),_IY[2],_IY[3]],new T(function(){return B(_N(_b,new T(function(){return B(_IV(_IX[2]));},1)));})];}};return B(_af(_Hu,B(_HP(B(_IV(B(_nS(_el,_BZ,_Hq,_Hs,new T(function(){return B(_Ii(B(_IF(B(_I2(_IL))))));})))))))));}),[1,new T(function(){var _IZ=B(_Ii(B(_IF(B(_I2(_IL))))));if(!_IZ[0]){var _J0=B(_af(_HB,B(_HP(_qH))));}else{var _J0=B(_af(_HB,B(_HP(B(_q7([1,[0,_IZ[1],_Hs,_IZ[2]],_b]))))));}var _J1=_J0,_J2=_J1;return _J2;}),[1,new T(function(){var _J3=B(_Ii(B(_IF(B(_I2(_IL))))));if(!_J3[0]){var _J4=B(_af(_Hw,B(_HP(_zQ))));}else{var _J4=B(_af(_Hw,B(_HP(B(_zC([1,[0,_J3[1],_Hs,_J3[2]],_b]))))));}var _J5=_J4,_J6=_J5;return _J6;}),[1,new T(function(){return B(_af(_Ht,B(_HP(B(_oJ(B(_nS(_7x,function(_xR,_oQ,_oR){return new F(function(){return _gw(_yw,_vt,_xR,_oQ,_oR);});},_Hq,_Hs,new T(function(){return B(_Ii(B(_IF(B(_I2(_IL))))));})))))))));}),[1,new T(function(){return B(_af(_Hv,B(_HP(B(_z4(_Hq,_Hs,B(_Ii(B(_IF(B(_I2(_IL))))))))))));}),[1,new T(function(){var _J7=function(_J8){while(1){var _J9=(function(_Ja){var _Jb=E(_Ja);if(!_Jb[0]){return [0];}else{var _Jc=_Jb[2],_Jd=E(_Jb[1]),_Je=B(_rn(_Jd[1],_Hq,_Jd[2],_Jd[3]));if(!_Je[0]){_J8=_Jc;return null;}else{return [1,_Je[1],new T(function(){return B(_N(_Je[2],new T(function(){return B(_J7(_Jc));},1)));})];}}})(_J8);if(_J9!=null){return _J9;}}},_Jf=B(_Ii(B(_IF(B(_I2(_IL))))));if(!_Jf[0]){var _Jg=B(_af(_Hz,B(_HP(B(_J7(_uV))))));}else{var _Jg=B(_af(_Hz,B(_HP(B(_J7(B(_uk([1,[0,_Jf[1],_Hs,_Jf[2]],_b]))))))));}var _Jh=_Jg,_Ji=_Jh;return _Ji;}),[1,new T(function(){return B(_af(_Hy,B(_HP(B(_qJ(_Hq,_Hs,B(_Ii(B(_IF(B(_I2(_IL))))))))))));}),[1,new T(function(){var _Jj=B(_Ii(B(_IF(B(_I2(_IL))))));if(!_Jj[0]){var _Jk=B(_af(_HC,B(_HP(_uV))));}else{var _Jk=B(_af(_HC,B(_HP(B(_uk([1,[0,_Jj[1],_Hs,_Jj[2]],_b]))))));}var _Jl=_Jk,_Jm=_Jl;return _Jm;}),[1,new T(function(){return B(_af(_Hx,B(_HP(B(A(_Hc,[_Hq,_Hs,new T(function(){return B(_Ii(B(_IF(B(_I2(_IL))))));})]))))));}),new T(function(){var _Jn=function(_Jo,_Jp){return [1,new T(function(){var _Jq=function(_Jr){var _Js=E(_Jr);return _Js[0]==0?E(new T(function(){var _Jt=function(_Ju){var _Jv=E(_Ju);return _Jv[0]==0?E(new T(function(){var _Jw=function(_Jx){var _Jy=E(_Jx);return _Jy[0]==0?E(new T(function(){var _Jz=function(_JA){var _JB=E(_JA);return _JB[0]==0?E(new T(function(){var _JC=function(_JD){var _JE=E(_JD);return _JE[0]==0?E(new T(function(){return B(_af(_HO,B(_HP(B(_He(B(_x4(_sT,_Jo,_Hq,_Hs,B(_Ii(B(_IF(B(_I2(_IL))))))))))))));})):[1,[0,_uM,_JE[1]],new T(function(){return B(_JC(_JE[2]));})];};return B(_JC(B(_HP(B(_Hi(B(_x4(_tz,_Jo,_Hq,_Hs,B(_Ii(B(_IF(B(_I2(_IL))))))))))))));})):[1,[0,_Hd,_JB[1]],new T(function(){return B(_Jz(_JB[2]));})];};return B(_Jz(new T(function(){return B(_HP(_b));})));})):[1,[0,_uH,_Jy[1]],new T(function(){return B(_Jw(_Jy[2]));})];};return B(_Jw(B(_HP(B(_Hm(B(_x4(_uf,_Jo,_Hq,_Hs,B(_Ii(B(_IF(B(_I2(_IL))))))))))))));})):[1,[0,_gL,_Jv[1]],new T(function(){return B(_Jt(_Jv[2]));})];};return B(_Jt(B(_HP(B(_yc(_Jo,_Hq,_Hs,B(_Ii(B(_IF(B(_I2(_IL))))))))))));})):[1,[0,_7p,_Js[1]],new T(function(){return B(_Jq(_Js[2]));})];};return B(_Jq(B(_HP(B(_xS(_Jo,_Hq,_Hs,new T(function(){return B(_Ii(B(_IF(B(_I2(_IL))))));})))))));}),_Jp];},_JF=function(_JG){var _JH=E(_JG);if(!_JH[0]){return E(new T(function(){var _JI=function(_JJ){var _JK=E(_JJ);if(!_JK[0]){return [0];}else{return new F(function(){return _Jn(new T(function(){return B(A(_JK[1],[_5y]));}),new T(function(){return B(_JI(_JK[2]));}));});}};return B(_JI(_HL));}));}else{return new F(function(){return _Jn(_JH[1],new T(function(){return B(_JF(_JH[2]));}));});}};return B(_JF(_HN));})]]]]]]]]]]);});},_JL=[1,_b,_b],_JM=new T(function(){return B(unCStr("------------------------------------------------------"));}),_JN=[1,_JM,_JL],_JO=[1,_b,_JN],_JP=new T(function(){return B(_1L(_b));}),_JQ=[0,10],_JR=function(_JS){var _JT=E(_JS);if(!_JT[0]){return [0];}else{return new F(function(){return _N(_JT[1],[1,_JQ,new T(function(){return B(_JR(_JT[2]));})]);});}},_JU=new T(function(){return B(_JR(_JP));}),_JV=new T(function(){return B(unCStr("(parse error)"));}),_JW=[1,_JV,_b],_JX=new T(function(){return B(_JR(_JW));}),_JY=function(_JZ){var _K0=B(_IK(_JZ));if(!_K0[0]){return E(_JX);}else{var _K1=B(_af(_gt,_K0));if(!_K1[0]){return E(_JU);}else{return new F(function(){return _JR(B(_1L([1,_K1[1],new T(function(){return B(_aj(_JO,_K1[2]));})])));});}}},_K2=function(_K3,_){var _K4=E(_K3);if(!_K4[0]){return E(_1J);}else{var _K5=E(_K4[2]);if(!_K5[0]){return E(_1J);}else{var _K6=E(_K5[2]);if(!_K6[0]){return E(_1J);}else{var _K7=_K6[1],_K8=E(_K6[2]);if(!_K8[0]){return E(_1J);}else{var _K9=E(_K8[2]);if(!_K9[0]){return E(_1J);}else{if(!E(_K9[2])[0]){var _Ka=function(_Kb,_){var _Kc=E(_K8[1])[1],_Kd=jsClearChildren(_Kc),_Ke=jsCreateTextNode(toJSStr(B(_JY(_Kb)))),_Kf=_Ke,_Kg=jsAppendChild(_Kf,_Kc);return _j;},_Kh=E(_K7)[1],_Ki=jsSetCB(_Kh,E(_k)[1],function(_Kj,_){switch(E(E(_Kj)[1])){case 10:var _Kk=jsGet(E(_K7)[1],toJSStr(E(_1K))),_Kl=_Kk;return new F(function(){return _Ka(new T(function(){return fromJSStr(_Kl);}),_);});break;case 13:var _Km=jsGet(E(_K7)[1],toJSStr(E(_1K))),_Kn=_Km;return new F(function(){return _Ka(new T(function(){return fromJSStr(_Kn);}),_);});break;default:return _j;}}),_Ko=_Ki,_Kp=E(_l)[1],_Kq=jsSetCB(E(_K4[1])[1],_Kp,function(_Kr,_Ks,_){var _Kt=jsGet(_Kh,toJSStr(E(_1K))),_Ku=_Kt;return new F(function(){return _Ka(new T(function(){return fromJSStr(_Ku);}),_);});}),_Kv=_Kq,_Kw=jsSetCB(E(_K5[1])[1],_Kp,function(_Kx,_Ky,_){var _Kz=jsGet(E(_K9[1])[1],toJSStr(E(_1K))),_KA=_Kz;return new F(function(){return _Ka(new T(function(){return fromJSStr(_KA);}),_);});}),_KB=_Kw;return _j;}else{return E(_1J);}}}}}}},_KC=new T(function(){return B(unCStr("button-translate-input"));}),_KD=new T(function(){return B(unCStr("button-translate-sample"));}),_KE=new T(function(){return B(unCStr("input"));}),_KF=new T(function(){return B(unCStr("sample"));}),_KG=[1,_KF,_b],_KH=new T(function(){return B(unCStr("output"));}),_KI=[1,_KH,_KG],_KJ=[1,_KE,_KI],_KK=[1,_KD,_KJ],_KL=[1,_KC,_KK],_KM=function(_KN){return [0,toJSStr(E(_KN))];},_KO=new T(function(){return B(_af(_KM,_KL));}),_KP=function(_KQ,_KR){while(1){var _KS=(function(_KT,_KU){var _KV=E(_KT);if(!_KV[0]){return [0];}else{var _KW=_KV[2],_KX=E(_KU);if(!_KX[0]){return [0];}else{var _KY=_KX[2];if(!E(_KX[1])[0]){return [1,_KV[1],new T(function(){return B(_KP(_KW,_KY));})];}else{_KQ=_KW;_KR=_KY;return null;}}}})(_KQ,_KR);if(_KS!=null){return _KS;}}},_KZ=new T(function(){return B(unAppCStr("[]",_b));}),_L0=[1,_V,_b],_L1=function(_L2){var _L3=E(_L2);return _L3[0]==0?E(_L0):[1,_U,new T(function(){return B(_N(fromJSStr(E(_L3[1])[1]),new T(function(){return B(_L1(_L3[2]));},1)));})];},_L4=function(_L5,_L6){return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _L7=B(_KP(_L6,_L5));return _L7[0]==0?E(_KZ):[1,_W,new T(function(){return B(_N(fromJSStr(E(_L7[1])[1]),new T(function(){return B(_L1(_L7[2]));},1)));})];}))));});},_L8=function(_){var _L9=B(_c(_KO,_)),_La=_L9;if(!B(_0(_9,_La))){return new F(function(){return _K2(B(_af(_6,_La)),_);});}else{return new F(function(){return _L4(_La,_KO);});}},_Lb=function(_){return new F(function(){return _L8(_);});};
var hasteMain = function() {B(A(_Lb, [0]));};window.onload = hasteMain;