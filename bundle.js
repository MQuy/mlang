(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory();
	else if(typeof define === 'function' && define.amd)
		define([], factory);
	else {
		var a = factory();
		for(var i in a) (typeof exports === 'object' ? exports : root)[i] = a[i];
	}
})(this, function() {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./mq.ts");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./mq.ts":
/*!***************!*\
  !*** ./mq.ts ***!
  \***************/
/*! exports provided: Lexer, Parser */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony import */ var _src_lexer__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./src/lexer */ "./src/lexer.ts");
/* harmony reexport (safe) */ __webpack_require__.d(__webpack_exports__, "Lexer", function() { return _src_lexer__WEBPACK_IMPORTED_MODULE_0__["Lexer"]; });

/* harmony import */ var _src_parser__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./src/parser */ "./src/parser.ts");
/* harmony reexport (safe) */ __webpack_require__.d(__webpack_exports__, "Parser", function() { return _src_parser__WEBPACK_IMPORTED_MODULE_1__["Parser"]; });






/***/ }),

/***/ "./src/lexer.ts":
/*!**********************!*\
  !*** ./src/lexer.ts ***!
  \**********************/
/*! exports provided: Lexer */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Lexer", function() { return Lexer; });
/* harmony import */ var _token__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./token */ "./src/token.ts");

class Lexer {
    constructor(source) {
        this.source = source;
        this.cPointer = 0;
    }
    execute() {
        const tokens = [];
        let char = this.source[this.cPointer];
        while (char) {
            if (char === undefined) {
                break;
            }
            else if (/[\d\.]/.test(char)) {
                tokens.push({ value: this.extractNumber(), type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].NUMBER });
                char = this.source[this.cPointer];
            }
            else {
                if (char === "+") {
                    tokens.push({ value: char, type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].PLUS });
                }
                else if (char === "-") {
                    tokens.push({ value: char, type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].MINUS });
                }
                else if (char === "*") {
                    tokens.push({ value: char, type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].MUL });
                }
                else if (char === "/") {
                    tokens.push({ value: char, type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].DIV });
                }
                else if (char === "(") {
                    tokens.push({ value: char, type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].OPEN_PAREN });
                }
                else if (char === ")") {
                    tokens.push({ value: char, type: _token__WEBPACK_IMPORTED_MODULE_0__["TType"].CLOSE_PAREN });
                }
                else if (!/\s/.test(char)) {
                    throw Error("Wrong syntax");
                }
                char = this.source[++this.cPointer];
            }
        }
        return tokens;
    }
    extractNumber() {
        let char = this.source[this.cPointer];
        let number = "";
        while (/[\d\.]/.test(char)) {
            number += char;
            char = this.source[++this.cPointer];
        }
        return parseFloat(number);
    }
}


/***/ }),

/***/ "./src/parser.ts":
/*!***********************!*\
  !*** ./src/parser.ts ***!
  \***********************/
/*! exports provided: Parser */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "Parser", function() { return Parser; });
/* harmony import */ var _token__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./token */ "./src/token.ts");

/**
 * expression = factor ((PLUS|MINUS) factor)*
 * factor = term ((MUL|DIV) term) *
 * term = NUMBER | OPEN_PAREN expression CLOSE_PAREN | (PLUS|MINUS) expression
 */
class Parser {
    constructor(tokens) {
        this.tokens = tokens;
        this.cPointer = 0;
    }
    expression() {
        let node = this.factor();
        let token = this.currentToken;
        while (token && (token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].PLUS || token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].MINUS)) {
            this.cPointer += 1;
            node = { left: node, operator: token, right: this.factor() };
            token = this.currentToken;
        }
        return node;
    }
    factor() {
        let node = this.term();
        let token = this.currentToken;
        while (token && (token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].MUL || token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].DIV)) {
            this.cPointer += 1;
            node = { left: node, operator: token, right: this.factor() };
            token = this.currentToken;
        }
        return node;
    }
    term() {
        let token = this.currentToken;
        if (token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].OPEN_PAREN) {
            this.cPointer += 1;
            const node = this.expression();
            this.cPointer += 1;
            return node;
        }
        else if (token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].PLUS || token.type === _token__WEBPACK_IMPORTED_MODULE_0__["TType"].MINUS) {
            this.cPointer += 1;
            return { operator: token, right: this.expression() };
        }
        else {
            this.cPointer += 1;
            return token;
        }
    }
    get currentToken() {
        return this.tokens[this.cPointer];
    }
    get nextToken() {
        return this.tokens[this.cPointer + 1];
    }
}


/***/ }),

/***/ "./src/token.ts":
/*!**********************!*\
  !*** ./src/token.ts ***!
  \**********************/
/*! exports provided: TType */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "TType", function() { return TType; });
var TType;
(function (TType) {
    TType[TType["NUMBER"] = 0] = "NUMBER";
    TType[TType["PLUS"] = 1] = "PLUS";
    TType[TType["MINUS"] = 2] = "MINUS";
    TType[TType["MUL"] = 3] = "MUL";
    TType[TType["DIV"] = 4] = "DIV";
    TType[TType["OPEN_PAREN"] = 5] = "OPEN_PAREN";
    TType[TType["CLOSE_PAREN"] = 6] = "CLOSE_PAREN";
})(TType || (TType = {}));


/***/ })

/******/ });
});
//# sourceMappingURL=bundle.js.map