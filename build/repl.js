'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

function _interopDefault (ex) { return (ex && (typeof ex === 'object') && 'default' in ex) ? ex['default'] : ex; }

var fs = _interopDefault(require('fs'));
var child_process = _interopDefault(require('child_process'));
var path = _interopDefault(require('path'));
var os = _interopDefault(require('os'));
var crypto = _interopDefault(require('crypto'));

function createCommonjsModule(fn, module) {
	return module = { exports: {} }, fn(module, module.exports), module.exports;
}

var readlineSync = createCommonjsModule(function (module, exports) {

var
  IS_WIN = process.platform === 'win32',

  ALGORITHM_CIPHER = 'aes-256-cbc',
  ALGORITHM_HASH = 'sha256',
  DEFAULT_ERR_MSG = 'The current environment doesn\'t support interactive reading from TTY.',
  TTY = process.binding('tty_wrap').TTY,

  defaultOptions = {
    /* eslint-disable key-spacing */
    prompt:             '> ',
    hideEchoBack:       false,
    mask:               '*',
    limit:              [],
    limitMessage:       'Input another, please.$<( [)limit(])>',
    defaultInput:       '',
    trueValue:          [],
    falseValue:         [],
    caseSensitive:      false,
    keepWhitespace:     false,
    encoding:           'utf8',
    bufferSize:         1024,
    print:              void 0,
    history:            true,
    cd:                 false,
    phContent:          void 0,
    preCheck:           void 0
    /* eslint-enable key-spacing */
  },

  fdR = 'none', fdW, ttyR, isRawMode = false,
  extHostPath, extHostArgs, tempdir, salt = 0,
  lastInput = '', inputHistory = [], rawInput,
  _DBG_useExt = false, _DBG_checkOptions = false, _DBG_checkMethod = false;

function getHostArgs(options) {
  // Send any text to crazy Windows shell safely.
  function encodeArg(arg) {
    return arg.replace(/[^\w\u0080-\uFFFF]/g, function(chr) {
      return '#' + chr.charCodeAt(0) + ';';
    });
  }

  return extHostArgs.concat((function(conf) {
    var args = [];
    Object.keys(conf).forEach(function(optionName) {
      if (conf[optionName] === 'boolean') {
        if (options[optionName]) { args.push('--' + optionName); }
      } else if (conf[optionName] === 'string') {
        if (options[optionName]) {
          args.push('--' + optionName, encodeArg(options[optionName]));
        }
      }
    });
    return args;
  })({
    /* eslint-disable key-spacing */
    display:        'string',
    displayOnly:    'boolean',
    keyIn:          'boolean',
    hideEchoBack:   'boolean',
    mask:           'string',
    limit:          'string',
    caseSensitive:  'boolean'
    /* eslint-enable key-spacing */
  }));
}

// piping via files (for Node.js v0.10-)
function _execFileSync(options, execOptions) {

  function getTempfile(name) {
    var filepath, suffix = '', fd;
    tempdir = tempdir || os.tmpdir();

    while (true) {
      filepath = path.join(tempdir, name + suffix);
      try {
        fd = fs.openSync(filepath, 'wx');
      } catch (e) {
        if (e.code === 'EEXIST') {
          suffix++;
          continue;
        } else {
          throw e;
        }
      }
      fs.closeSync(fd);
      break;
    }
    return filepath;
  }

  var hostArgs, shellPath, shellArgs, res = {}, exitCode, extMessage,
    pathStdout = getTempfile('readline-sync.stdout'),
    pathStderr = getTempfile('readline-sync.stderr'),
    pathExit = getTempfile('readline-sync.exit'),
    pathDone = getTempfile('readline-sync.done'),
    crypto$$1 = crypto, shasum, decipher, password;

  shasum = crypto$$1.createHash(ALGORITHM_HASH);
  shasum.update('' + process.pid + (salt++) + Math.random());
  password = shasum.digest('hex');
  decipher = crypto$$1.createDecipher(ALGORITHM_CIPHER, password);

  hostArgs = getHostArgs(options);
  if (IS_WIN) {
    shellPath = process.env.ComSpec || 'cmd.exe';
    process.env.Q = '"'; // The quote (") that isn't escaped.
    // `()` for ignore space by echo
    shellArgs = ['/V:ON', '/S', '/C',
      '(%Q%' + shellPath + '%Q% /V:ON /S /C %Q%' + /* ESLint bug? */ // eslint-disable-line no-path-concat
        '%Q%' + extHostPath + '%Q%' +
          hostArgs.map(function(arg) { return ' %Q%' + arg + '%Q%'; }).join('') +
        ' & (echo !ERRORLEVEL!)>%Q%' + pathExit + '%Q%%Q%) 2>%Q%' + pathStderr + '%Q%' +
      ' |%Q%' + process.execPath + '%Q% %Q%' + __dirname + '\\encrypt.js%Q%' +
        ' %Q%' + ALGORITHM_CIPHER + '%Q% %Q%' + password + '%Q%' +
        ' >%Q%' + pathStdout + '%Q%' +
      ' & (echo 1)>%Q%' + pathDone + '%Q%'];
  } else {
    shellPath = '/bin/sh';
    shellArgs = ['-c',
      // Use `()`, not `{}` for `-c` (text param)
      '("' + extHostPath + '"' + /* ESLint bug? */ // eslint-disable-line no-path-concat
          hostArgs.map(function(arg) { return " '" + arg.replace(/'/g, "'\\''") + "'"; }).join('') +
        '; echo $?>"' + pathExit + '") 2>"' + pathStderr + '"' +
      ' |"' + process.execPath + '" "' + __dirname + '/encrypt.js"' +
        ' "' + ALGORITHM_CIPHER + '" "' + password + '"' +
        ' >"' + pathStdout + '"' +
      '; echo 1 >"' + pathDone + '"'];
  }
  if (_DBG_checkMethod) { _DBG_checkMethod('_execFileSync', hostArgs); }
  try {
    child_process.spawn(shellPath, shellArgs, execOptions);
  } catch (e) {
    res.error = new Error(e.message);
    res.error.method = '_execFileSync - spawn';
    res.error.program = shellPath;
    res.error.args = shellArgs;
  }

  while (fs.readFileSync(pathDone, {encoding: options.encoding}).trim() !== '1') {} // eslint-disable-line no-empty
  if ((exitCode =
      fs.readFileSync(pathExit, {encoding: options.encoding}).trim()) === '0') {
    res.input =
      decipher.update(fs.readFileSync(pathStdout, {encoding: 'binary'}),
        'hex', options.encoding) +
      decipher.final(options.encoding);
  } else {
    extMessage = fs.readFileSync(pathStderr, {encoding: options.encoding}).trim();
    res.error = new Error(DEFAULT_ERR_MSG + (extMessage ? '\n' + extMessage : ''));
    res.error.method = '_execFileSync';
    res.error.program = shellPath;
    res.error.args = shellArgs;
    res.error.extMessage = extMessage;
    res.error.exitCode = +exitCode;
  }

  fs.unlinkSync(pathStdout);
  fs.unlinkSync(pathStderr);
  fs.unlinkSync(pathExit);
  fs.unlinkSync(pathDone);

  return res;
}

function readlineExt(options) {
  var hostArgs, res = {}, extMessage,
    execOptions = {env: process.env, encoding: options.encoding};

  if (!extHostPath) {
    if (IS_WIN) {
      if (process.env.PSModulePath) { // Windows PowerShell
        extHostPath = 'powershell.exe';
        extHostArgs = ['-ExecutionPolicy', 'Bypass', '-File', __dirname + '\\read.ps1']; // eslint-disable-line no-path-concat
      } else {                        // Windows Script Host
        extHostPath = 'cscript.exe';
        extHostArgs = ['//nologo', __dirname + '\\read.cs.js']; // eslint-disable-line no-path-concat
      }
    } else {
      extHostPath = '/bin/sh';
      extHostArgs = [__dirname + '/read.sh']; // eslint-disable-line no-path-concat
    }
  }
  if (IS_WIN && !process.env.PSModulePath) { // Windows Script Host
    // ScriptPW (Win XP and Server2003) needs TTY stream as STDIN.
    // In this case, If STDIN isn't TTY, an error is thrown.
    execOptions.stdio = [process.stdin];
  }

  if (child_process.execFileSync) {
    hostArgs = getHostArgs(options);
    if (_DBG_checkMethod) { _DBG_checkMethod('execFileSync', hostArgs); }
    try {
      res.input = child_process.execFileSync(extHostPath, hostArgs, execOptions);
    } catch (e) { // non-zero exit code
      extMessage = e.stderr ? (e.stderr + '').trim() : '';
      res.error = new Error(DEFAULT_ERR_MSG + (extMessage ? '\n' + extMessage : ''));
      res.error.method = 'execFileSync';
      res.error.program = extHostPath;
      res.error.args = hostArgs;
      res.error.extMessage = extMessage;
      res.error.exitCode = e.status;
      res.error.code = e.code;
      res.error.signal = e.signal;
    }
  } else {
    res = _execFileSync(options, execOptions);
  }
  if (!res.error) {
    res.input = res.input.replace(/^\s*'|'\s*$/g, '');
    options.display = '';
  }

  return res;
}

/*
  display:            string
  displayOnly:        boolean
  keyIn:              boolean
  hideEchoBack:       boolean
  mask:               string
  limit:              string (pattern)
  caseSensitive:      boolean
  keepWhitespace:     boolean
  encoding, bufferSize, print
*/
function _readlineSync(options) {
  var input = '', displaySave = options.display,
    silent = !options.display &&
      options.keyIn && options.hideEchoBack && !options.mask;

  function tryExt() {
    var res = readlineExt(options);
    if (res.error) { throw res.error; }
    return res.input;
  }

  if (_DBG_checkOptions) { _DBG_checkOptions(options); }

  (function() { // open TTY
    var fsB, constants, verNum;

    function getFsB() {
      if (!fsB) {
        fsB = process.binding('fs'); // For raw device path
        constants = process.binding('constants');
      }
      return fsB;
    }

    if (typeof fdR !== 'string') { return; }
    fdR = null;

    if (IS_WIN) {
      // iojs-v2.3.2+ input stream can't read first line. (#18)
      // ** Don't get process.stdin before check! **
      // Fixed v5.1.0
      // Fixed v4.2.4
      // It regressed again in v5.6.0, it is fixed in v6.2.0.
      verNum = (function(ver) { // getVerNum
        var nums = ver.replace(/^\D+/, '').split('.');
        var verNum = 0;
        if ((nums[0] = +nums[0])) { verNum += nums[0] * 10000; }
        if ((nums[1] = +nums[1])) { verNum += nums[1] * 100; }
        if ((nums[2] = +nums[2])) { verNum += nums[2]; }
        return verNum;
      })(process.version);
      if (!(verNum >= 20302 && verNum < 40204 || verNum >= 50000 && verNum < 50100 || verNum >= 50600 && verNum < 60200) &&
          process.stdin.isTTY) {
        process.stdin.pause();
        fdR = process.stdin.fd;
        ttyR = process.stdin._handle;
      } else {
        try {
          // The stream by fs.openSync('\\\\.\\CON', 'r') can't switch to raw mode.
          // 'CONIN$' might fail on XP, 2000, 7 (x86).
          fdR = getFsB().open('CONIN$', constants.O_RDWR, parseInt('0666', 8));
          ttyR = new TTY(fdR, true);
        } catch (e) { /* ignore */ }
      }

      if (process.stdout.isTTY) {
        fdW = process.stdout.fd;
      } else {
        try {
          fdW = fs.openSync('\\\\.\\CON', 'w');
        } catch (e) { /* ignore */ }
        if (typeof fdW !== 'number') { // Retry
          try {
            fdW = getFsB().open('CONOUT$', constants.O_RDWR, parseInt('0666', 8));
          } catch (e) { /* ignore */ }
        }
      }

    } else {
      if (process.stdin.isTTY) {
        process.stdin.pause();
        try {
          fdR = fs.openSync('/dev/tty', 'r'); // device file, not process.stdin
          ttyR = process.stdin._handle;
        } catch (e) { /* ignore */ }
      } else {
        // Node.js v0.12 read() fails.
        try {
          fdR = fs.openSync('/dev/tty', 'r');
          ttyR = new TTY(fdR, false);
        } catch (e) { /* ignore */ }
      }

      if (process.stdout.isTTY) {
        fdW = process.stdout.fd;
      } else {
        try {
          fdW = fs.openSync('/dev/tty', 'w');
        } catch (e) { /* ignore */ }
      }
    }
  })();

  (function() { // try read
    var atEol, limit,
      isCooked = !options.hideEchoBack && !options.keyIn,
      buffer, reqSize, readSize, chunk, line;
    rawInput = '';

    // Node.js v0.10- returns an error if same mode is set.
    function setRawMode(mode) {
      if (mode === isRawMode) { return true; }
      if (ttyR.setRawMode(mode) !== 0) { return false; }
      isRawMode = mode;
      return true;
    }

    if (_DBG_useExt || !ttyR ||
        typeof fdW !== 'number' && (options.display || !isCooked)) {
      input = tryExt();
      return;
    }

    if (options.display) {
      fs.writeSync(fdW, options.display);
      options.display = '';
    }
    if (options.displayOnly) { return; }

    if (!setRawMode(!isCooked)) {
      input = tryExt();
      return;
    }

    reqSize = options.keyIn ? 1 : options.bufferSize;
    // Check `allocUnsafe` to make sure of the new API.
    buffer = Buffer.allocUnsafe && Buffer.alloc ? Buffer.alloc(reqSize) : new Buffer(reqSize);

    if (options.keyIn && options.limit) {
      limit = new RegExp('[^' + options.limit + ']',
        'g' + (options.caseSensitive ? '' : 'i'));
    }

    while (true) {
      readSize = 0;
      try {
        readSize = fs.readSync(fdR, buffer, 0, reqSize);
      } catch (e) {
        if (e.code !== 'EOF') {
          setRawMode(false);
          input += tryExt();
          return;
        }
      }
      if (readSize > 0) {
        chunk = buffer.toString(options.encoding, 0, readSize);
        rawInput += chunk;
      } else {
        chunk = '\n';
        rawInput += String.fromCharCode(0);
      }

      if (chunk && typeof (line = (chunk.match(/^(.*?)[\r\n]/) || [])[1]) === 'string') {
        chunk = line;
        atEol = true;
      }

      // other ctrl-chars
      // eslint-disable-next-line no-control-regex
      if (chunk) { chunk = chunk.replace(/[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]/g, ''); }
      if (chunk && limit) { chunk = chunk.replace(limit, ''); }

      if (chunk) {
        if (!isCooked) {
          if (!options.hideEchoBack) {
            fs.writeSync(fdW, chunk);
          } else if (options.mask) {
            fs.writeSync(fdW, (new Array(chunk.length + 1)).join(options.mask));
          }
        }
        input += chunk;
      }

      if (!options.keyIn && atEol ||
        options.keyIn && input.length >= reqSize) { break; }
    }

    if (!isCooked && !silent) { fs.writeSync(fdW, '\n'); }
    setRawMode(false);
  })();

  if (options.print && !silent) {
    options.print(displaySave + (options.displayOnly ? '' :
        (options.hideEchoBack ? (new Array(input.length + 1)).join(options.mask)
          : input) + '\n'), // must at least write '\n'
      options.encoding);
  }

  return options.displayOnly ? '' :
    (lastInput = options.keepWhitespace || options.keyIn ? input : input.trim());
}

function flattenArray(array, validator) {
  var flatArray = [];
  function _flattenArray(array) {
    if (array == null) {
      return;
    } else if (Array.isArray(array)) {
      array.forEach(_flattenArray);
    } else if (!validator || validator(array)) {
      flatArray.push(array);
    }
  }
  _flattenArray(array);
  return flatArray;
}

function escapePattern(pattern) {
  return pattern.replace(/[\x00-\x7f]/g, // eslint-disable-line no-control-regex
    function(s) { return '\\x' + ('00' + s.charCodeAt().toString(16)).substr(-2); });
}

// margeOptions(options1, options2 ... )
// margeOptions(true, options1, options2 ... )
//    arg1=true : Start from defaultOptions and pick elements of that.
function margeOptions() {
  var optionsList = Array.prototype.slice.call(arguments),
    optionNames, fromDefault;

  if (optionsList.length && typeof optionsList[0] === 'boolean') {
    fromDefault = optionsList.shift();
    if (fromDefault) {
      optionNames = Object.keys(defaultOptions);
      optionsList.unshift(defaultOptions);
    }
  }

  return optionsList.reduce(function(options, optionsPart) {
    if (optionsPart == null) { return options; }

    // ======== DEPRECATED ========
    if (optionsPart.hasOwnProperty('noEchoBack') &&
        !optionsPart.hasOwnProperty('hideEchoBack')) {
      optionsPart.hideEchoBack = optionsPart.noEchoBack;
      delete optionsPart.noEchoBack;
    }
    if (optionsPart.hasOwnProperty('noTrim') &&
        !optionsPart.hasOwnProperty('keepWhitespace')) {
      optionsPart.keepWhitespace = optionsPart.noTrim;
      delete optionsPart.noTrim;
    }
    // ======== /DEPRECATED ========

    if (!fromDefault) { optionNames = Object.keys(optionsPart); }
    optionNames.forEach(function(optionName) {
      var value;
      if (!optionsPart.hasOwnProperty(optionName)) { return; }
      value = optionsPart[optionName];
      switch (optionName) {
                           // _readlineSync <- *    * -> defaultOptions
        // ================ string
        case 'mask':                        // *    *
        case 'limitMessage':                //      *
        case 'defaultInput':                //      *
        case 'encoding':                    // *    *
          value = value != null ? value + '' : '';
          if (value && optionName !== 'limitMessage') { value = value.replace(/[\r\n]/g, ''); }
          options[optionName] = value;
          break;
        // ================ number(int)
        case 'bufferSize':                  // *    *
          if (!isNaN(value = parseInt(value, 10)) && typeof value === 'number') {
            options[optionName] = value; // limited updating (number is needed)
          }
          break;
        // ================ boolean
        case 'displayOnly':                 // *
        case 'keyIn':                       // *
        case 'hideEchoBack':                // *    *
        case 'caseSensitive':               // *    *
        case 'keepWhitespace':              // *    *
        case 'history':                     //      *
        case 'cd':                          //      *
          options[optionName] = !!value;
          break;
        // ================ array
        case 'limit':                       // *    *     to string for readlineExt
        case 'trueValue':                   //      *
        case 'falseValue':                  //      *
          options[optionName] = flattenArray(value, function(value) {
            var type = typeof value;
            return type === 'string' || type === 'number' ||
              type === 'function' || value instanceof RegExp;
          }).map(function(value) {
            return typeof value === 'string' ? value.replace(/[\r\n]/g, '') : value;
          });
          break;
        // ================ function
        case 'print':                       // *    *
        case 'phContent':                   //      *
        case 'preCheck':                    //      *
          options[optionName] = typeof value === 'function' ? value : void 0;
          break;
        // ================ other
        case 'prompt':                      //      *
        case 'display':                     // *
          options[optionName] = value != null ? value : '';
          break;
        // no default
      }
    });
    return options;
  }, {});
}

function isMatched(res, comps, caseSensitive) {
  return comps.some(function(comp) {
    var type = typeof comp;
    return type === 'string' ?
        (caseSensitive ? res === comp : res.toLowerCase() === comp.toLowerCase()) :
      type === 'number' ? parseFloat(res) === comp :
      type === 'function' ? comp(res) :
      comp instanceof RegExp ? comp.test(res) : false;
  });
}

function replaceHomePath(path$$1, expand) {
  var homePath = path.normalize(
    IS_WIN ? (process.env.HOMEDRIVE || '') + (process.env.HOMEPATH || '') :
    process.env.HOME || '').replace(/[\/\\]+$/, '');
  path$$1 = path.normalize(path$$1);
  return expand ? path$$1.replace(/^~(?=\/|\\|$)/, homePath) :
    path$$1.replace(new RegExp('^' + escapePattern(homePath) +
      '(?=\\/|\\\\|$)', IS_WIN ? 'i' : ''), '~');
}

function replacePlaceholder(text, generator) {
  var PTN_INNER = '(?:\\(([\\s\\S]*?)\\))?(\\w+|.-.)(?:\\(([\\s\\S]*?)\\))?',
    rePlaceholder = new RegExp('(\\$)?(\\$<' + PTN_INNER + '>)', 'g'),
    rePlaceholderCompat = new RegExp('(\\$)?(\\$\\{' + PTN_INNER + '\\})', 'g');

  function getPlaceholderText(s, escape, placeholder, pre, param, post) {
    var text;
    return escape || typeof (text = generator(param)) !== 'string' ? placeholder :
      text ? (pre || '') + text + (post || '') : '';
  }

  return text.replace(rePlaceholder, getPlaceholderText)
    .replace(rePlaceholderCompat, getPlaceholderText);
}

function array2charlist(array, caseSensitive, collectSymbols) {
  var values, group = [], groupClass = -1, charCode = 0, symbols = '', suppressed;
  function addGroup(groups, group) {
    if (group.length > 3) { // ellipsis
      groups.push(group[0] + '...' + group[group.length - 1]);
      suppressed = true;
    } else if (group.length) {
      groups = groups.concat(group);
    }
    return groups;
  }

  values = array.reduce(
      function(chars, value) { return chars.concat((value + '').split('')); }, [])
    .reduce(function(groups, curChar) {
      var curGroupClass, curCharCode;
      if (!caseSensitive) { curChar = curChar.toLowerCase(); }
      curGroupClass = /^\d$/.test(curChar) ? 1 :
        /^[A-Z]$/.test(curChar) ? 2 : /^[a-z]$/.test(curChar) ? 3 : 0;
      if (collectSymbols && curGroupClass === 0) {
        symbols += curChar;
      } else {
        curCharCode = curChar.charCodeAt(0);
        if (curGroupClass && curGroupClass === groupClass &&
            curCharCode === charCode + 1) {
          group.push(curChar);
        } else {
          groups = addGroup(groups, group);
          group = [curChar];
          groupClass = curGroupClass;
        }
        charCode = curCharCode;
      }
      return groups;
    }, []);
  values = addGroup(values, group); // last group
  if (symbols) { values.push(symbols); suppressed = true; }
  return {values: values, suppressed: suppressed};
}

function joinChunks(chunks, suppressed) {
  return chunks.join(chunks.length > 2 ? ', ' : suppressed ? ' / ' : '/');
}

function getPhContent(param, options) {
  var text, values, resCharlist = {}, arg;
  if (options.phContent) {
    text = options.phContent(param, options);
  }
  if (typeof text !== 'string') {
    switch (param) {
      case 'hideEchoBack':
      case 'mask':
      case 'defaultInput':
      case 'caseSensitive':
      case 'keepWhitespace':
      case 'encoding':
      case 'bufferSize':
      case 'history':
      case 'cd':
        text = !options.hasOwnProperty(param) ? '' :
          typeof options[param] === 'boolean' ? (options[param] ? 'on' : 'off') :
          options[param] + '';
        break;
      // case 'prompt':
      // case 'query':
      // case 'display':
      //   text = options.hasOwnProperty('displaySrc') ? options.displaySrc + '' : '';
      //   break;
      case 'limit':
      case 'trueValue':
      case 'falseValue':
        values = options[options.hasOwnProperty(param + 'Src') ? param + 'Src' : param];
        if (options.keyIn) { // suppress
          resCharlist = array2charlist(values, options.caseSensitive);
          values = resCharlist.values;
        } else {
          values = values.filter(function(value) {
            var type = typeof value;
            return type === 'string' || type === 'number';
          });
        }
        text = joinChunks(values, resCharlist.suppressed);
        break;
      case 'limitCount':
      case 'limitCountNotZero':
        text = options[options.hasOwnProperty('limitSrc') ?
          'limitSrc' : 'limit'].length;
        text = text || param !== 'limitCountNotZero' ? text + '' : '';
        break;
      case 'lastInput':
        text = lastInput;
        break;
      case 'cwd':
      case 'CWD':
      case 'cwdHome':
        text = process.cwd();
        if (param === 'CWD') {
          text = path.basename(text);
        } else if (param === 'cwdHome') {
          text = replaceHomePath(text);
        }
        break;
      case 'date':
      case 'time':
      case 'localeDate':
      case 'localeTime':
        text = (new Date())['to' +
          param.replace(/^./, function(str) { return str.toUpperCase(); }) +
          'String']();
        break;
      default: // with arg
        if (typeof (arg = (param.match(/^history_m(\d+)$/) || [])[1]) === 'string') {
          text = inputHistory[inputHistory.length - arg] || '';
        }
    }
  }
  return text;
}

function getPhCharlist(param) {
  var matches = /^(.)-(.)$/.exec(param), text = '', from, to, code, step;
  if (!matches) { return null; }
  from = matches[1].charCodeAt(0);
  to = matches[2].charCodeAt(0);
  step = from < to ? 1 : -1;
  for (code = from; code !== to + step; code += step) { text += String.fromCharCode(code); }
  return text;
}

// cmd "arg" " a r g " "" 'a"r"g' "a""rg" "arg
function parseCl(cl) {
  var reToken = new RegExp(/(\s*)(?:("|')(.*?)(?:\2|$)|(\S+))/g), matches,
    taken = '', args = [], part;
  cl = cl.trim();
  while ((matches = reToken.exec(cl))) {
    part = matches[3] || matches[4] || '';
    if (matches[1]) {
      args.push(taken);
      taken = '';
    }
    taken += part;
  }
  if (taken) { args.push(taken); }
  return args;
}

function toBool(res, options) {
  return (
    (options.trueValue.length &&
      isMatched(res, options.trueValue, options.caseSensitive)) ? true :
    (options.falseValue.length &&
      isMatched(res, options.falseValue, options.caseSensitive)) ? false : res);
}

function getValidLine(options) {
  var res, forceNext, limitMessage,
    matches, histInput, args, resCheck;

  function _getPhContent(param) { return getPhContent(param, options); }
  function addDisplay(text) { options.display += (/[^\r\n]$/.test(options.display) ? '\n' : '') + text; }

  options.limitSrc = options.limit;
  options.displaySrc = options.display;
  options.limit = ''; // for readlineExt
  options.display = replacePlaceholder(options.display + '', _getPhContent);

  while (true) {
    res = _readlineSync(options);
    forceNext = false;
    limitMessage = '';

    if (options.defaultInput && !res) { res = options.defaultInput; }

    if (options.history) {
      if ((matches = /^\s*\!(?:\!|-1)(:p)?\s*$/.exec(res))) { // `!!` `!-1` +`:p`
        histInput = inputHistory[0] || '';
        if (matches[1]) { // only display
          forceNext = true;
        } else { // replace input
          res = histInput;
        }
        // Show it even if it is empty (NL only).
        addDisplay(histInput + '\n');
        if (!forceNext) { // Loop may break
          options.displayOnly = true;
          _readlineSync(options);
          options.displayOnly = false;
        }
      } else if (res && res !== inputHistory[inputHistory.length - 1]) {
        inputHistory = [res];
      }
    }

    if (!forceNext && options.cd && res) {
      args = parseCl(res);
      switch (args[0].toLowerCase()) {
        case 'cd':
          if (args[1]) {
            try {
              process.chdir(replaceHomePath(args[1], true));
            } catch (e) {
              addDisplay(e + '');
            }
          }
          forceNext = true;
          break;
        case 'pwd':
          addDisplay(process.cwd());
          forceNext = true;
          break;
        // no default
      }
    }

    if (!forceNext && options.preCheck) {
      resCheck = options.preCheck(res, options);
      res = resCheck.res;
      if (resCheck.forceNext) { forceNext = true; } // Don't switch to false.
    }

    if (!forceNext) {
      if (!options.limitSrc.length ||
        isMatched(res, options.limitSrc, options.caseSensitive)) { break; }
      if (options.limitMessage) {
        limitMessage = replacePlaceholder(options.limitMessage, _getPhContent);
      }
    }

    addDisplay((limitMessage ? limitMessage + '\n' : '') +
      replacePlaceholder(options.displaySrc + '', _getPhContent));
  }
  return toBool(res, options);
}

// for dev
exports._DBG_set_useExt = function(val) { _DBG_useExt = val; };
exports._DBG_set_checkOptions = function(val) { _DBG_checkOptions = val; };
exports._DBG_set_checkMethod = function(val) { _DBG_checkMethod = val; };
exports._DBG_clearHistory = function() { lastInput = ''; inputHistory = []; };

// ------------------------------------

exports.setDefaultOptions = function(options) {
  defaultOptions = margeOptions(true, options);
  return margeOptions(true); // copy
};

exports.question = function(query, options) {
  /* eslint-disable key-spacing */
  return getValidLine(margeOptions(margeOptions(true, options), {
    display:            query
  }));
  /* eslint-enable key-spacing */
};

exports.prompt = function(options) {
  var readOptions = margeOptions(true, options);
  readOptions.display = readOptions.prompt;
  return getValidLine(readOptions);
};

exports.keyIn = function(query, options) {
  /* eslint-disable key-spacing */
  var readOptions = margeOptions(margeOptions(true, options), {
    display:            query,
    keyIn:              true,
    keepWhitespace:     true
  });
  /* eslint-enable key-spacing */

  // char list
  readOptions.limitSrc = readOptions.limit.filter(function(value) {
    var type = typeof value;
    return type === 'string' || type === 'number';
  })
  .map(function(text) { return replacePlaceholder(text + '', getPhCharlist); });
  // pattern
  readOptions.limit = escapePattern(readOptions.limitSrc.join(''));

  ['trueValue', 'falseValue'].forEach(function(optionName) {
    readOptions[optionName] = readOptions[optionName].reduce(function(comps, comp) {
      var type = typeof comp;
      if (type === 'string' || type === 'number') {
        comps = comps.concat((comp + '').split(''));
      } else { comps.push(comp); }
      return comps;
    }, []);
  });

  readOptions.display = replacePlaceholder(readOptions.display + '',
    function(param) { return getPhContent(param, readOptions); });

  return toBool(_readlineSync(readOptions), readOptions);
};

// ------------------------------------

exports.questionEMail = function(query, options) {
  if (query == null) { query = 'Input e-mail address: '; }
  /* eslint-disable key-spacing */
  return exports.question(query, margeOptions({
    // -------- default
    hideEchoBack:       false,
    // http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
    limit:              /^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/,
    limitMessage:       'Input valid e-mail address, please.',
    trueValue:          null,
    falseValue:         null
  }, options, {
    // -------- forced
    keepWhitespace:     false,
    cd:                 false
  }));
  /* eslint-enable key-spacing */
};

exports.questionNewPassword = function(query, options) {
  /* eslint-disable key-spacing */
  var resCharlist, min, max,
    readOptions = margeOptions({
      // -------- default
      hideEchoBack:       true,
      mask:               '*',
      limitMessage:       'It can include: $<charlist>\n' +
                            'And the length must be: $<length>',
      trueValue:          null,
      falseValue:         null,
      caseSensitive:      true
    }, options, {
      // -------- forced
      history:            false,
      cd:                 false,
      // limit (by charlist etc.),
      phContent: function(param) {
        return param === 'charlist' ? resCharlist.text :
          param === 'length' ? min + '...' + max : null;
      }
    }),
    // added:     charlist, min, max, confirmMessage, unmatchMessage
    charlist, confirmMessage, unmatchMessage,
    limit, limitMessage, res1, res2;
  /* eslint-enable key-spacing */
  options = options || {};

  charlist = replacePlaceholder(
    options.charlist ? options.charlist + '' : '$<!-~>', getPhCharlist);
  if (isNaN(min = parseInt(options.min, 10)) || typeof min !== 'number') { min = 12; }
  if (isNaN(max = parseInt(options.max, 10)) || typeof max !== 'number') { max = 24; }
  limit = new RegExp('^[' + escapePattern(charlist) +
    ']{' + min + ',' + max + '}$');
  resCharlist = array2charlist([charlist], readOptions.caseSensitive, true);
  resCharlist.text = joinChunks(resCharlist.values, resCharlist.suppressed);

  confirmMessage = options.confirmMessage != null ? options.confirmMessage :
    'Reinput a same one to confirm it: ';
  unmatchMessage = options.unmatchMessage != null ? options.unmatchMessage :
    'It differs from first one.' +
      ' Hit only the Enter key if you want to retry from first one.';

  if (query == null) { query = 'Input new password: '; }

  limitMessage = readOptions.limitMessage;
  while (!res2) {
    readOptions.limit = limit;
    readOptions.limitMessage = limitMessage;
    res1 = exports.question(query, readOptions);

    readOptions.limit = [res1, ''];
    readOptions.limitMessage = unmatchMessage;
    res2 = exports.question(confirmMessage, readOptions);
  }

  return res1;
};

function _questionNum(query, options, parser) {
  var validValue;
  function getValidValue(value) {
    validValue = parser(value);
    return !isNaN(validValue) && typeof validValue === 'number';
  }
  /* eslint-disable key-spacing */
  exports.question(query, margeOptions({
    // -------- default
    limitMessage:       'Input valid number, please.'
  }, options, {
    // -------- forced
    limit:              getValidValue,
    cd:                 false
    // trueValue, falseValue, caseSensitive, keepWhitespace don't work.
  }));
  /* eslint-enable key-spacing */
  return validValue;
}
exports.questionInt = function(query, options) {
  return _questionNum(query, options, function(value) { return parseInt(value, 10); });
};
exports.questionFloat = function(query, options) {
  return _questionNum(query, options, parseFloat);
};

exports.questionPath = function(query, options) {
  /* eslint-disable key-spacing */
  var validPath, error = '',
    readOptions = margeOptions({
      // -------- default
      hideEchoBack:       false,
      limitMessage:       '$<error(\n)>Input valid path, please.' +
                            '$<( Min:)min>$<( Max:)max>',
      history:            true,
      cd:                 true
    }, options, {
      // -------- forced
      keepWhitespace:     false,
      limit: function(value) {
        var exists, stat, res;
        value = replaceHomePath(value, true);
        error = ''; // for validate
        // mkdir -p
        function mkdirParents(dirPath) {
          dirPath.split(/\/|\\/).reduce(function(parents, dir) {
            var path$$1 = path.resolve((parents += dir + path.sep));
            if (!fs.existsSync(path$$1)) {
              fs.mkdirSync(path$$1);
            } else if (!fs.statSync(path$$1).isDirectory()) {
              throw new Error('Non directory already exists: ' + path$$1);
            }
            return parents;
          }, '');
        }

        try {
          exists = fs.existsSync(value);
          validPath = exists ? fs.realpathSync(value) : path.resolve(value);
          // options.exists default: true, not-bool: no-check
          if (!options.hasOwnProperty('exists') && !exists ||
              typeof options.exists === 'boolean' && options.exists !== exists) {
            error = (exists ? 'Already exists' : 'No such file or directory') +
              ': ' + validPath;
            return false;
          }
          if (!exists && options.create) {
            if (options.isDirectory) {
              mkdirParents(validPath);
            } else {
              mkdirParents(path.dirname(validPath));
              fs.closeSync(fs.openSync(validPath, 'w')); // touch
            }
            validPath = fs.realpathSync(validPath);
          }
          if (exists && (options.min || options.max ||
              options.isFile || options.isDirectory)) {
            stat = fs.statSync(validPath);
            // type check first (directory has zero size)
            if (options.isFile && !stat.isFile()) {
              error = 'Not file: ' + validPath;
              return false;
            } else if (options.isDirectory && !stat.isDirectory()) {
              error = 'Not directory: ' + validPath;
              return false;
            } else if (options.min && stat.size < +options.min ||
                options.max && stat.size > +options.max) {
              error = 'Size ' + stat.size + ' is out of range: ' + validPath;
              return false;
            }
          }
          if (typeof options.validate === 'function' &&
              (res = options.validate(validPath)) !== true) {
            if (typeof res === 'string') { error = res; }
            return false;
          }
        } catch (e) {
          error = e + '';
          return false;
        }
        return true;
      },
      // trueValue, falseValue, caseSensitive don't work.
      phContent: function(param) {
        return param === 'error' ? error :
          param !== 'min' && param !== 'max' ? null :
          options.hasOwnProperty(param) ? options[param] + '' : '';
      }
    });
    // added:     exists, create, min, max, isFile, isDirectory, validate
  /* eslint-enable key-spacing */
  options = options || {};

  if (query == null) { query = 'Input path (you can "cd" and "pwd"): '; }

  exports.question(query, readOptions);
  return validPath;
};

// props: preCheck, args, hRes, limit
function getClHandler(commandHandler, options) {
  var clHandler = {}, hIndex = {};
  if (typeof commandHandler === 'object') {
    Object.keys(commandHandler).forEach(function(cmd) {
      if (typeof commandHandler[cmd] === 'function') {
        hIndex[options.caseSensitive ? cmd : cmd.toLowerCase()] = commandHandler[cmd];
      }
    });
    clHandler.preCheck = function(res) {
      var cmdKey;
      clHandler.args = parseCl(res);
      cmdKey = clHandler.args[0] || '';
      if (!options.caseSensitive) { cmdKey = cmdKey.toLowerCase(); }
      clHandler.hRes =
        cmdKey !== '_' && hIndex.hasOwnProperty(cmdKey) ?
          hIndex[cmdKey].apply(res, clHandler.args.slice(1)) :
        hIndex.hasOwnProperty('_') ? hIndex._.apply(res, clHandler.args) : null;
      return {res: res, forceNext: false};
    };
    if (!hIndex.hasOwnProperty('_')) {
      clHandler.limit = function() { // It's called after preCheck.
        var cmdKey = clHandler.args[0] || '';
        if (!options.caseSensitive) { cmdKey = cmdKey.toLowerCase(); }
        return hIndex.hasOwnProperty(cmdKey);
      };
    }
  } else {
    clHandler.preCheck = function(res) {
      clHandler.args = parseCl(res);
      clHandler.hRes = typeof commandHandler === 'function' ?
        commandHandler.apply(res, clHandler.args) : true; // true for break loop
      return {res: res, forceNext: false};
    };
  }
  return clHandler;
}

exports.promptCL = function(commandHandler, options) {
  /* eslint-disable key-spacing */
  var readOptions = margeOptions({
      // -------- default
      hideEchoBack:       false,
      limitMessage:       'Requested command is not available.',
      caseSensitive:      false,
      history:            true
    }, options),
      // -------- forced
      // trueValue, falseValue, keepWhitespace don't work.
      // preCheck, limit (by clHandler)
    clHandler = getClHandler(commandHandler, readOptions);
  /* eslint-enable key-spacing */
  readOptions.limit = clHandler.limit;
  readOptions.preCheck = clHandler.preCheck;
  exports.prompt(readOptions);
  return clHandler.args;
};

exports.promptLoop = function(inputHandler, options) {
  /* eslint-disable key-spacing */
  var readOptions = margeOptions({
    // -------- default
    hideEchoBack:       false,
    trueValue:          null,
    falseValue:         null,
    caseSensitive:      false,
    history:            true
  }, options);
  /* eslint-enable key-spacing */
  while (true) { if (inputHandler(exports.prompt(readOptions))) { break; } }
  return;
};

exports.promptCLLoop = function(commandHandler, options) {
  /* eslint-disable key-spacing */
  var readOptions = margeOptions({
      // -------- default
      hideEchoBack:       false,
      limitMessage:       'Requested command is not available.',
      caseSensitive:      false,
      history:            true
    }, options),
      // -------- forced
      // trueValue, falseValue, keepWhitespace don't work.
      // preCheck, limit (by clHandler)
    clHandler = getClHandler(commandHandler, readOptions);
  /* eslint-enable key-spacing */
  readOptions.limit = clHandler.limit;
  readOptions.preCheck = clHandler.preCheck;
  while (true) {
    exports.prompt(readOptions);
    if (clHandler.hRes) { break; }
  }
  return;
};

exports.promptSimShell = function(options) {
  /* eslint-disable key-spacing */
  return exports.prompt(margeOptions({
    // -------- default
    hideEchoBack:       false,
    history:            true
  }, options, {
    // -------- forced
    prompt:             (function() {
      return IS_WIN ?
        '$<cwd>>' :
        // 'user@host:cwd$ '
        (process.env.USER || '') +
        (process.env.HOSTNAME ?
          '@' + process.env.HOSTNAME.replace(/\..*$/, '') : '') +
        ':$<cwdHome>$ ';
    })()
  }));
  /* eslint-enable key-spacing */
};

function _keyInYN(query, options, limit) {
  var res;
  if (query == null) { query = 'Are you sure? '; }
  if ((!options || options.guide !== false) && (query += '')) {
    query = query.replace(/\s*:?\s*$/, '') + ' [y/n]: ';
  }
  /* eslint-disable key-spacing */
  res = exports.keyIn(query, margeOptions(options, {
    // -------- forced
    hideEchoBack:       false,
    limit:              limit,
    trueValue:          'y',
    falseValue:         'n',
    caseSensitive:      false
    // mask doesn't work.
  }));
  // added:     guide
  /* eslint-enable key-spacing */
  return typeof res === 'boolean' ? res : '';
}
exports.keyInYN = function(query, options) { return _keyInYN(query, options); };
exports.keyInYNStrict = function(query, options) { return _keyInYN(query, options, 'yn'); };

exports.keyInPause = function(query, options) {
  if (query == null) { query = 'Continue...'; }
  if ((!options || options.guide !== false) && (query += '')) {
    query = query.replace(/\s+$/, '') + ' (Hit any key)';
  }
  /* eslint-disable key-spacing */
  exports.keyIn(query, margeOptions({
    // -------- default
    limit:              null
  }, options, {
    // -------- forced
    hideEchoBack:       true,
    mask:               ''
  }));
  // added:     guide
  /* eslint-enable key-spacing */
  return;
};

exports.keyInSelect = function(items, query, options) {
  /* eslint-disable key-spacing */
  var readOptions = margeOptions({
      // -------- default
      hideEchoBack:       false
    }, options, {
      // -------- forced
      trueValue:          null,
      falseValue:         null,
      caseSensitive:      false,
      // limit (by items),
      phContent: function(param) {
        return param === 'itemsCount' ? items.length + '' :
          param === 'firstItem' ? (items[0] + '').trim() :
          param === 'lastItem' ? (items[items.length - 1] + '').trim() : null;
      }
    }),
    // added:     guide, cancel
    keylist = '', key2i = {}, charCode = 49 /* '1' */, display = '\n';
  /* eslint-enable key-spacing */
  if (!Array.isArray(items) || !items.length || items.length > 35) {
    throw '`items` must be Array (max length: 35).';
  }

  items.forEach(function(item, i) {
    var key = String.fromCharCode(charCode);
    keylist += key;
    key2i[key] = i;
    display += '[' + key + '] ' + (item + '').trim() + '\n';
    charCode = charCode === 57 /* '9' */ ? 97 /* 'a' */ : charCode + 1;
  });
  if (!options || options.cancel !== false) {
    keylist += '0';
    key2i['0'] = -1;
    display += '[0] ' +
      (options && options.cancel != null && typeof options.cancel !== 'boolean' ?
        (options.cancel + '').trim() : 'CANCEL') + '\n';
  }
  readOptions.limit = keylist;
  display += '\n';

  if (query == null) { query = 'Choose one from list: '; }
  if ((query += '')) {
    if (!options || options.guide !== false) {
      query = query.replace(/\s*:?\s*$/, '') + ' [$<limit>]: ';
    }
    display += query;
  }

  return key2i[exports.keyIn(display, readOptions).toLowerCase()];
};

exports.getRawInput = function() { return rawInput; };

// ======== DEPRECATED ========
function _setOption(optionName, args) {
  var options;
  if (args.length) { options = {}; options[optionName] = args[0]; }
  return exports.setDefaultOptions(options)[optionName];
}
exports.setPrint = function() { return _setOption('print', arguments); };
exports.setPrompt = function() { return _setOption('prompt', arguments); };
exports.setEncoding = function() { return _setOption('encoding', arguments); };
exports.setMask = function() { return _setOption('mask', arguments); };
exports.setBufferSize = function() { return _setOption('bufferSize', arguments); };
});
var readlineSync_1 = readlineSync._DBG_set_useExt;
var readlineSync_2 = readlineSync._DBG_set_checkOptions;
var readlineSync_3 = readlineSync._DBG_set_checkMethod;
var readlineSync_4 = readlineSync._DBG_clearHistory;
var readlineSync_5 = readlineSync.setDefaultOptions;
var readlineSync_6 = readlineSync.question;
var readlineSync_7 = readlineSync.prompt;
var readlineSync_8 = readlineSync.keyIn;
var readlineSync_9 = readlineSync.questionEMail;
var readlineSync_10 = readlineSync.questionNewPassword;
var readlineSync_11 = readlineSync.questionInt;
var readlineSync_12 = readlineSync.questionFloat;
var readlineSync_13 = readlineSync.questionPath;
var readlineSync_14 = readlineSync.promptCL;
var readlineSync_15 = readlineSync.promptLoop;
var readlineSync_16 = readlineSync.promptCLLoop;
var readlineSync_17 = readlineSync.promptSimShell;
var readlineSync_18 = readlineSync.keyInYN;
var readlineSync_19 = readlineSync.keyInYNStrict;
var readlineSync_20 = readlineSync.keyInPause;
var readlineSync_21 = readlineSync.keyInSelect;
var readlineSync_22 = readlineSync.getRawInput;
var readlineSync_23 = readlineSync.setPrint;
var readlineSync_24 = readlineSync.setPrompt;
var readlineSync_25 = readlineSync.setEncoding;
var readlineSync_26 = readlineSync.setMask;
var readlineSync_27 = readlineSync.setBufferSize;

var TokenType;
(function (TokenType) {
    TokenType["LEFT_PAREN"] = "LEFT_PAREN";
    TokenType["RIGHT_PAREN"] = "RIGHT_PAREN";
    TokenType["LEFT_BRACE"] = "LEFT_BRACE";
    TokenType["RIGHT_BRACE"] = "RIGHT_BRACE";
    TokenType["LEFT_BRACKET"] = "LEFT_BRACKET";
    TokenType["RIGHT_BRACKET"] = "RIGHT_BRACKET";
    TokenType["DEF"] = "DEF";
    TokenType["CLASS"] = "CLASS";
    TokenType["EXTENDS"] = "EXTENDS";
    TokenType["VAR"] = "VAR";
    TokenType["NEW"] = "NEW";
    TokenType["ARROW"] = "ARROW";
    TokenType["IF"] = "IF";
    TokenType["ELSE"] = "ELSE";
    TokenType["FOR"] = "FOR";
    TokenType["WHILE"] = "WHILE";
    TokenType["BREAK"] = "BREAK";
    TokenType["CONTINUE"] = "CONTINUE";
    TokenType["RETURN"] = "RETURN";
    TokenType["COMMA"] = "COMMA";
    TokenType["SEMICOLON"] = "SEMICOLON";
    TokenType["COLON"] = "COLON";
    TokenType["DOT"] = "DOT";
    TokenType["EQUAL"] = "EQUAL";
    TokenType["BANG_EQUAL"] = "BANG_EQUAL";
    TokenType["EQUAL_EQUAL"] = "EQUAL_EQUAL";
    TokenType["GREAT"] = "GREAT";
    TokenType["GREAT_THAN"] = "GREAT_THAN";
    TokenType["LESS"] = "LESS";
    TokenType["LESS_THAN"] = "LESS_THAN";
    TokenType["AND"] = "AND";
    TokenType["OR"] = "OR";
    TokenType["PLUS"] = "PLUS";
    TokenType["MINUS"] = "MINUS";
    TokenType["STAR"] = "STAR";
    TokenType["STAR_STAR"] = "STAR_STAR";
    TokenType["SLASH"] = "SLASH";
    TokenType["PLUS_PLUS"] = "PLUS_PLUS";
    TokenType["MINUS_MINUS"] = "MINUS_MINUS";
    TokenType["BANG"] = "BANG";
    TokenType["NUMBER"] = "NUMBER";
    TokenType["STRING"] = "STRING";
    TokenType["BOOLEAN"] = "BOOLEAN";
    TokenType["NULL"] = "NULL";
    TokenType["THIS"] = "THIS";
    TokenType["SUPER"] = "SUPER";
    TokenType["IDENTIFIER"] = "IDENTIFIER";
    TokenType["COMMENT"] = "COMMENT";
    TokenType["EOF"] = "EOF";
})(TokenType || (TokenType = {}));
class Token {
    constructor(type, lexeme, literal, line, column) {
        this.type = type;
        this.lexeme = lexeme;
        this.literal = literal;
        this.line = line;
        this.column = column;
    }
    toString() {
        return `Line ${this.line}:${this.column}`;
    }
}

const reservedWords = {
    def: TokenType.DEF,
    class: TokenType.CLASS,
    var: TokenType.VAR,
    extends: TokenType.EXTENDS,
    new: TokenType.NEW,
    if: TokenType.IF,
    else: TokenType.ELSE,
    for: TokenType.FOR,
    while: TokenType.WHILE,
    break: TokenType.BREAK,
    continue: TokenType.CONTINUE,
    return: TokenType.RETURN,
    and: TokenType.AND,
    or: TokenType.OR,
    null: TokenType.NULL,
    this: TokenType.THIS,
    super: TokenType.SUPER,
    true: TokenType.BOOLEAN,
    false: TokenType.BOOLEAN,
};
class Lexer {
    constructor(source) {
        this.source = source;
    }
    scan() {
        const tokens = [];
        this.runner = 0;
        this.line = 1;
        this.column = 1;
        while (this.notAtEnd()) {
            this.current = this.runner;
            const token = this.scanToken();
            if (token)
                tokens.push(token);
        }
        tokens.push(new Token(TokenType.EOF, "", undefined, this.line, this.column));
        return tokens;
    }
    scanToken() {
        const char = this.advance();
        switch (char) {
            case "{":
                return this.generateToken(TokenType.LEFT_BRACE);
            case "}":
                return this.generateToken(TokenType.RIGHT_BRACE);
            case "(":
                return this.generateToken(TokenType.LEFT_PAREN);
            case ")":
                return this.generateToken(TokenType.RIGHT_PAREN);
            case "[":
                return this.generateToken(TokenType.LEFT_BRACKET);
            case "]":
                return this.generateToken(TokenType.RIGHT_BRACKET);
            case ".":
                return this.generateToken(TokenType.DOT);
            case ";":
                return this.generateToken(TokenType.SEMICOLON);
            case ",":
                return this.generateToken(TokenType.COMMA);
            case ":":
                return this.generateToken(TokenType.COLON);
            case "/":
                if (this.match("/")) {
                    return this.comment();
                }
                else {
                    return this.generateToken(TokenType.SLASH);
                }
            case "*":
                if (this.match("*")) {
                    return this.generateToken(TokenType.STAR_STAR);
                }
                else {
                    return this.generateToken(TokenType.STAR);
                }
            case "+":
                if (this.match("+")) {
                    return this.generateToken(TokenType.PLUS_PLUS);
                }
                else {
                    return this.generateToken(TokenType.PLUS);
                }
            case "-":
                if (this.match("-")) {
                    return this.generateToken(TokenType.MINUS_MINUS);
                }
                else {
                    return this.generateToken(TokenType.MINUS);
                }
            case "=":
                if (this.match("=")) {
                    return this.generateToken(TokenType.EQUAL_EQUAL);
                }
                else if (this.match(">")) {
                    return this.generateToken(TokenType.ARROW);
                }
                else {
                    return this.generateToken(TokenType.EQUAL);
                }
            case "!":
                if (this.match("=")) {
                    return this.generateToken(TokenType.BANG_EQUAL);
                }
                else {
                    return this.generateToken(TokenType.BANG);
                }
            case ">":
                if (this.match("=")) {
                    return this.generateToken(TokenType.GREAT_THAN);
                }
                else {
                    return this.generateToken(TokenType.GREAT);
                }
            case "<":
                if (this.match("=")) {
                    return this.generateToken(TokenType.LESS_THAN);
                }
                else {
                    return this.generateToken(TokenType.LESS);
                }
            case "\n":
                this.newline();
                return;
            case '"':
                return this.string();
            default:
                if (this.isDigit(char)) {
                    return this.number();
                }
                else if (this.isAlpha(char)) {
                    return this.identifier();
                }
                else if (/^\s$/.test(char)) {
                    return;
                }
                else {
                    throw new Error(`${this.line}: Unexpected character ${char}.`);
                }
        }
    }
    comment() {
        while (this.peek() !== "\n" && this.notAtEnd())
            this.advance();
        this.newline();
        return this.generateToken(TokenType.COMMENT);
    }
    string() {
        while (this.peek() !== '"' && this.notAtEnd()) {
            if (this.peek() === "\n")
                this.newline();
            this.advance();
        }
        if (!this.notAtEnd()) {
            throw new Error(`${this.line}: Unterminated string.`);
        }
        this.advance();
        return this.generateToken(TokenType.STRING, this.getLexeme(this.current + 1, this.runner - 1));
    }
    number() {
        this.moveCursor(-1);
        while (/^[0-9]$/.test(this.peek()) && this.notAtEnd())
            this.advance();
        if (this.match(".")) {
            while (/[0-9]/.test(this.peek()) && this.notAtEnd())
                this.advance();
        }
        return this.generateToken(TokenType.NUMBER, parseFloat(this.getLexeme()));
    }
    identifier() {
        while (/^\w$/.test(this.peek()) && this.notAtEnd())
            this.advance();
        const lexeme = this.getLexeme();
        if (Object.keys(reservedWords).includes(lexeme)) {
            const word = reservedWords[lexeme];
            if (word === TokenType.BOOLEAN) {
                return this.generateToken(word, lexeme === "true");
            }
            else if (word === TokenType.NULL) {
                return this.generateToken(word, null);
            }
            else {
                return this.generateToken(word);
            }
        }
        else {
            return this.generateToken(TokenType.IDENTIFIER);
        }
    }
    generateToken(type, literal) {
        return new Token(type, this.getLexeme(), literal, this.line, this.column - (this.runner - this.current));
    }
    getLexeme(start = this.current, end = this.runner) {
        return this.source.substring(start, end);
    }
    isDigit(char) {
        return /^[0-9]$/.test(char);
    }
    isAlpha(char) {
        return /^\w$/.test(char);
    }
    match(char) {
        if (this.peek() !== char)
            return false;
        this.moveCursor(1);
        return true;
    }
    newline() {
        this.line += 1;
        this.column = 1;
    }
    peek() {
        return this.source[this.runner];
    }
    advance() {
        return this.source[this.moveCursor(1)];
    }
    moveCursor(distance) {
        const current = this.runner;
        this.column += distance;
        this.runner += distance;
        return current;
    }
    notAtEnd() {
        return this.runner < this.source.length;
    }
}

class IRNode {
}

class IfStatement extends IRNode {
    constructor(condition, thenStatement, elseStatement) {
        super();
        this.condition = condition;
        this.thenStatement = thenStatement;
        this.elseStatement = elseStatement;
    }
    accept(visitor) {
        return visitor.visitIfStatement(this);
    }
}
class BlockStatement extends IRNode {
    constructor(statements) {
        super();
        this.statements = statements;
    }
    accept(visitor) {
        return visitor.visitBlockStatement(this);
    }
}
class BreakStatement extends IRNode {
    accept(visitor) {
        return visitor.visitBreakStatement(this);
    }
}
class ContinueStatement extends IRNode {
    accept(visitor) {
        return visitor.visitContinueStatement(this);
    }
}
class ForStatement extends IRNode {
    constructor(body, condition, intializer, increment) {
        super();
        this.body = body;
        this.condition = condition;
        this.initializer = intializer;
        this.increment = increment;
    }
    accept(visitor) {
        return visitor.visitForStatement(this);
    }
}
class VarStatement extends IRNode {
    constructor(name, initializer, type) {
        super();
        this.name = name;
        this.initializer = initializer;
        this.type = type;
    }
    accept(visitor) {
        return visitor.visitVarStatement(this);
    }
}
class VarsStatement extends IRNode {
    constructor(varStatements) {
        super();
        this.varStatements = varStatements;
    }
    accept(visitor) {
        return visitor.visitVarsStatements(this);
    }
}
class ClassStatement extends IRNode {
    constructor(name, properties, methods, superclass) {
        super();
        this.name = name;
        this.superclass = superclass;
        this.properties = properties;
        this.methods = methods;
    }
    accept(visitor) {
        return visitor.visitClassStatement(this);
    }
}
class FunctionStatement extends IRNode {
    constructor(name, parameters, body, returnType) {
        super();
        this.name = name;
        this.parameters = parameters;
        this.body = body;
        this.returnType = returnType;
    }
    accept(visitor) {
        return visitor.visitFunctionStatement(this);
    }
}
class ParameterDeclaration {
    constructor(name, type) {
        this.name = name;
        this.type = type;
    }
}
class ReturnStatement extends IRNode {
    constructor(value) {
        super();
        this.value = value;
    }
    accept(visitor) {
        return visitor.visitReturnStatement(this);
    }
}
class EmptyStatement extends IRNode {
    accept(visitor) {
        return undefined;
    }
}
class ExpressionStatement extends IRNode {
    constructor(expression) {
        super();
        this.expression = expression;
    }
    accept(visitor) {
        return visitor.visitExpressionStatement(this);
    }
}

function error(token, errorMessage) {
    throw new Error(`Line ${token.line}:${token.column} ${errorMessage}`);
}

var BuiltinTypes;
(function (BuiltinTypes) {
    BuiltinTypes["String"] = "String";
    BuiltinTypes["Number"] = "Number";
    BuiltinTypes["Null"] = "Null";
    BuiltinTypes["Boolean"] = "Boolean";
})(BuiltinTypes || (BuiltinTypes = {}));
class Functionable {
    constructor(returnType, name) {
        this.name = name;
        this.parameters = [];
        this.returnType = returnType;
    }
}
class Classable {
    constructor(name) {
        this.name = name;
        this.properties = {};
        this.methods = {};
    }
    get(name) {
        if (Object.keys(this.properties).includes(name)) {
            return this.properties[name];
        }
        else if (Object.keys(this.methods).includes(name)) {
            return this.methods[name];
        }
        else {
            throw new Error(`${name} is not defined`);
        }
    }
}
function checkInheritance(type1, type2, position) {
    if (type1 === type2) {
        return true;
    }
    else if (type1 instanceof Classable && type2 instanceof Classable) {
        let superclass = type1.superclass;
        while (superclass) {
            if (superclass === type2) {
                return true;
            }
            superclass = superclass.superclass;
        }
    }
    error(position, `Type '${getTypeName(type1)}' is not assignable to type '${getTypeName(type2)}'`);
}
function getTypeName(type) {
    if (type instanceof Classable || type instanceof Functionable) {
        return type.name;
    }
    else {
        return type;
    }
}

class AssignmentExpression extends IRNode {
    constructor(name, expression) {
        super();
        this.name = name;
        this.expression = expression;
    }
    accept(visitor) {
        return visitor.visitAssignmentExpression(this);
    }
}
class LogicalExpression extends IRNode {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.operator = operator;
        this.right = right;
        this.type = BuiltinTypes.Boolean;
    }
    accept(visitor) {
        return visitor.visitLogicalExpression(this);
    }
}
class BinaryExpression extends IRNode {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
    accept(visitor) {
        return visitor.visitBinaryExpression(this);
    }
}
class UnaryExpression extends IRNode {
    constructor(operator, right) {
        super();
        this.operator = operator;
        this.right = right;
    }
    accept(visitor) {
        return visitor.visitUnaryExpression(this);
    }
}
class CallExpression extends IRNode {
    constructor(callee, args) {
        super();
        this.callee = callee;
        this.args = args;
    }
    accept(visitor) {
        return visitor.visitCallExpression(this);
    }
}
class SetExpression extends IRNode {
    constructor(object, name, value) {
        super();
        this.object = object;
        this.name = name;
        this.value = value;
    }
    accept(visitor) {
        visitor.visitSetExpression(this);
    }
}
class GetExpression extends IRNode {
    constructor(object, name) {
        super();
        this.object = object;
        this.name = name;
    }
    accept(visitor) {
        return visitor.visitGetExpression(this);
    }
}
class LiteralExpression extends IRNode {
    constructor(name) {
        super();
        this.name = name;
        const type = name.type.toLowerCase();
        this.type = (type.slice(0, 1).toUpperCase() + type.slice(1));
    }
    accept(visitor) {
        return visitor.visitLiteralExpression(this);
    }
}
class GroupExpression extends IRNode {
    constructor(expression) {
        super();
        this.expression = expression;
    }
    accept(visitor) {
        return visitor.visitGroupExpression(this);
    }
}
class LambdaExpression extends IRNode {
    constructor(parameters, body, returnType) {
        super();
        this.parameters = parameters;
        this.body = body;
        this.returnType = returnType;
    }
    accept(visitor) {
        return visitor.visitLambdaExpression(this);
    }
}
class NewExpression extends IRNode {
    constructor(name, args) {
        super();
        this.name = name;
        this.args = args;
    }
    accept(visitor) {
        return visitor.visitNewExpression(this);
    }
}
class ArrayExpression extends IRNode {
    constructor(elements) {
        super();
        this.elements = elements;
    }
    accept(visitor) {
        return visitor.visitArrayExpression(this);
    }
}
class ThisExpression extends IRNode {
    constructor(keyword) {
        super();
        this.keyword = keyword;
    }
    accept(visitor) {
        return visitor.visitThisExpression(this);
    }
}
class SuperExpression extends IRNode {
    constructor(keyword) {
        super();
        this.keyword = keyword;
    }
    accept(visitor) {
        return visitor.visitSuperExpression(this);
    }
}
class VarExpression extends IRNode {
    constructor(name) {
        super();
        this.name = name;
    }
    accept(visitor) {
        return visitor.visitVarExpression(this);
    }
}

class Program {
    constructor(statements) {
        this.statements = statements;
    }
}

class Parser {
    constructor(tokens) {
        this.tokens = tokens;
    }
    parse() {
        const statements = [];
        this.current = 0;
        while (this.notAtEnd()) {
            statements.push(this.statement());
        }
        return new Program(statements);
    }
    statement() {
        if (this.match(TokenType.IF)) {
            return this.ifStatement();
        }
        else if (this.match(TokenType.LEFT_BRACE)) {
            return this.blockStatement();
        }
        else if (this.match(TokenType.BREAK)) {
            return this.breakStatement();
        }
        else if (this.match(TokenType.CONTINUE)) {
            return this.continueStatement();
        }
        else if (this.match(TokenType.FOR)) {
            return this.forStatement();
        }
        else if (this.match(TokenType.WHILE)) {
            return this.whileStatement();
        }
        else if (this.match(TokenType.VAR)) {
            return this.varsStatement();
        }
        else if (this.match(TokenType.CLASS)) {
            return this.classStatement();
        }
        else if (this.match(TokenType.RETURN)) {
            return this.returnStatement();
        }
        else if (this.match(TokenType.SEMICOLON)) {
            return this.emptyStatement();
        }
        else if (this.check(TokenType.DEF) &&
            this.next().type === TokenType.IDENTIFIER) {
            this.consume(TokenType.DEF, "Expect def");
            return this.functionStatement();
        }
        else {
            return this.expressionStatement();
        }
    }
    ifStatement() {
        const ifToken = this.previous();
        this.consume(TokenType.LEFT_PAREN, "Expect ( after if");
        const condition = this.expression();
        this.consume(TokenType.RIGHT_PAREN, "Expect ) after if condition");
        const thenStatement = this.statement();
        let elseStatement;
        if (this.match(TokenType.ELSE)) {
            elseStatement = this.statement();
        }
        return this.generateStatement(new IfStatement(condition, thenStatement, elseStatement), ifToken);
    }
    blockStatement() {
        const blockToken = this.previous();
        const statements = [];
        while (!this.match(TokenType.RIGHT_BRACE)) {
            statements.push(this.statement());
        }
        return this.generateStatement(new BlockStatement(statements), blockToken);
    }
    breakStatement() {
        const breakToken = this.previous();
        this.consume(TokenType.SEMICOLON, "Expect ; after break");
        return this.generateStatement(new BreakStatement(), breakToken);
    }
    continueStatement() {
        const continueToken = this.previous();
        this.consume(TokenType.SEMICOLON, "Expect ; after continue");
        return this.generateStatement(new ContinueStatement(), continueToken);
    }
    forStatement() {
        const forToken = this.previous();
        this.consume(TokenType.LEFT_PAREN, "Expect ( after for");
        let initializer = [];
        if (!this.match(TokenType.SEMICOLON)) {
            if (this.match(TokenType.VAR)) {
                initializer = this.varStatement();
            }
            else if (this.check(TokenType.IDENTIFIER)) {
                initializer = [this.expression()];
                while (this.match(TokenType.COMMA)) {
                    initializer.push(this.expression());
                }
                this.consume(TokenType.SEMICOLON, "Expect ; after while intializer");
            }
            else {
                error(this.peek(), "Expect declaration or assignment");
            }
        }
        let condition;
        if (!this.match(TokenType.SEMICOLON)) {
            condition = this.expression();
            this.consume(TokenType.SEMICOLON, "Expect ; after condition");
        }
        let increment;
        if (!this.match(TokenType.RIGHT_PAREN)) {
            const incrementExpression = this.expression();
            increment = this.generateStatement(new ExpressionStatement(incrementExpression), incrementExpression.pStart);
            this.consume(TokenType.RIGHT_PAREN, "Expect )");
        }
        return this.generateStatement(new ForStatement(this.statement(), condition, initializer, increment), forToken);
    }
    whileStatement() {
        const whileToken = this.previous();
        this.consume(TokenType.LEFT_PAREN, "Expect ( after while");
        const condition = this.expression();
        this.consume(TokenType.RIGHT_PAREN, "Expect ) after condition");
        return this.generateStatement(new ForStatement(this.statement(), condition), whileToken);
    }
    varsStatement() {
        const varToken = this.previous();
        return this.generateStatement(new VarsStatement(this.varStatement()), varToken);
    }
    varStatement() {
        const statements = [];
        while (this.notAtEnd()) {
            const varToken = this.previous().type === TokenType.VAR ? this.previous() : this.peek();
            const name = this.consume(TokenType.IDENTIFIER, "Expect identifier after var");
            let intializer;
            let type;
            if (this.match(TokenType.COLON)) {
                const kind = this.consume(TokenType.IDENTIFIER, "Expect type after var");
                type = kind.lexeme;
            }
            if (this.match(TokenType.EQUAL)) {
                intializer = this.expression();
            }
            statements.push(this.generateStatement(new VarStatement(name, intializer, type), varToken, this.peek().type === TokenType.SEMICOLON
                ? this.peek()
                : this.previous()));
            if (this.match(TokenType.COMMA)) {
                continue;
            }
            else if (this.match(TokenType.SEMICOLON)) {
                break;
            }
            else {
                error(this.peek(), "Expect ; after declaration");
            }
        }
        return statements;
    }
    classStatement() {
        const classToken = this.previous();
        const name = this.consume(TokenType.IDENTIFIER, "Expect class name");
        let superclass;
        if (this.match(TokenType.EXTENDS)) {
            superclass = this.consume(TokenType.IDENTIFIER, "Expect super class name");
        }
        this.consume(TokenType.LEFT_BRACE, "Expect {");
        let varStatements = [];
        let methodStatements = [];
        while (!this.match(TokenType.RIGHT_BRACE)) {
            if (this.match(TokenType.VAR)) {
                varStatements = [...varStatements, ...this.varStatement()];
            }
            else if (this.match(TokenType.DEF)) {
                methodStatements = [...methodStatements, this.functionStatement()];
            }
            else {
                error(this.peek(), "Expect class properties or methods");
            }
        }
        return this.generateStatement(new ClassStatement(name, varStatements, methodStatements, superclass), classToken);
    }
    functionStatement() {
        const functionToken = this.previous();
        const name = this.consume(TokenType.IDENTIFIER, "Expect name");
        this.consume(TokenType.LEFT_PAREN, "Expect ( after name");
        const parameters = this.parameters();
        this.consume(TokenType.COLON, "Expect return type");
        const kind = this.consume(TokenType.IDENTIFIER, "Expect type after function");
        return this.generateStatement(new FunctionStatement(name, parameters, this.statement(), kind.lexeme), functionToken);
    }
    returnStatement() {
        const returnToken = this.previous();
        let expression;
        if (!this.match(TokenType.SEMICOLON)) {
            expression = this.expression();
            this.consume(TokenType.SEMICOLON, "Expect ; after return expression");
        }
        return this.generateStatement(new ReturnStatement(expression), returnToken);
    }
    emptyStatement() {
        return this.generateStatement(new EmptyStatement(), this.previous());
    }
    expressionStatement() {
        const expression = this.expression();
        this.consume(TokenType.SEMICOLON, "Expect ;");
        return this.generateStatement(new ExpressionStatement(expression), expression.pStart);
    }
    expression() {
        return this.assignmentExpression();
    }
    assignmentExpression() {
        let logical = this.logicalExpression();
        while (this.match(TokenType.EQUAL)) {
            if (logical instanceof GetExpression) {
                logical = this.generateExpression(new SetExpression(logical.object, logical.name, this.logicalExpression()), logical.pStart);
            }
            else if (logical instanceof VarExpression) {
                logical = this.generateExpression(new AssignmentExpression(logical.name, this.logicalExpression()), logical.pStart);
            }
        }
        return logical;
    }
    logicalExpression() {
        let equality = this.equalityExpression();
        while (this.match(TokenType.AND, TokenType.OR)) {
            equality = this.generateExpression(new LogicalExpression(equality, this.previous(), this.equalityExpression()), equality.pStart);
        }
        return equality;
    }
    equalityExpression() {
        let compare = this.compareExpression();
        while (this.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)) {
            compare = this.generateExpression(new LogicalExpression(compare, this.previous(), this.compareExpression()), compare.pStart);
        }
        return compare;
    }
    compareExpression() {
        let addition = this.additionExpression();
        while (this.match(TokenType.GREAT, TokenType.GREAT_THAN, TokenType.LESS, TokenType.LESS_THAN)) {
            addition = this.generateExpression(new LogicalExpression(addition, this.previous(), this.additionExpression()), addition.pStart);
        }
        return addition;
    }
    additionExpression() {
        let multiplication = this.multiplicationExpression();
        while (this.match(TokenType.PLUS, TokenType.MINUS)) {
            multiplication = this.generateExpression(new BinaryExpression(multiplication, this.previous(), this.multiplicationExpression()), multiplication.pStart);
        }
        return multiplication;
    }
    multiplicationExpression() {
        let exponentiation = this.exponentiationExpression();
        while (this.match(TokenType.STAR, TokenType.SLASH)) {
            exponentiation = this.generateExpression(new BinaryExpression(exponentiation, this.previous(), this.exponentiationExpression()), exponentiation.pStart);
        }
        return exponentiation;
    }
    exponentiationExpression() {
        let unary = this.unaryExpression();
        while (this.match(TokenType.STAR_STAR)) {
            unary = this.generateExpression(new BinaryExpression(unary, this.previous(), this.unaryExpression()), unary.pStart);
        }
        return unary;
    }
    unaryExpression() {
        if (this.match(TokenType.PLUS, TokenType.MINUS, TokenType.PLUS_PLUS, TokenType.MINUS_MINUS, TokenType.BANG)) {
            const unaryToken = this.previous();
            return this.generateExpression(new UnaryExpression(this.previous(), this.callExpression()), unaryToken);
        }
        return this.callExpression();
    }
    callExpression() {
        let member = this.memberAccessExpression();
        while (this.match(TokenType.LEFT_PAREN)) {
            const args = [];
            while (!this.match(TokenType.RIGHT_PAREN)) {
                args.push(this.expression());
                while (this.match(TokenType.COMMA)) {
                    args.push(this.expression());
                }
            }
            member = this.generateExpression(new CallExpression(member, args), member.pStart);
        }
        return member;
    }
    memberAccessExpression() {
        let primary = this.primaryExpression();
        while (this.match(TokenType.DOT)) {
            const name = this.consume(TokenType.IDENTIFIER, "Expect identifer after .");
            primary = this.generateExpression(new GetExpression(primary, name), primary.pStart);
        }
        return primary;
    }
    primaryExpression() {
        if (this.match(TokenType.BOOLEAN, TokenType.NUMBER, TokenType.STRING, TokenType.NULL)) {
            const literalToken = this.previous();
            return this.generateExpression(new LiteralExpression(literalToken), literalToken);
        }
        else if (this.match(TokenType.THIS)) {
            return this.generateExpression(new ThisExpression(this.previous()), this.previous());
        }
        else if (this.match(TokenType.SUPER)) {
            return this.generateExpression(new SuperExpression(this.previous()), this.previous());
        }
        else if (this.match(TokenType.LEFT_PAREN)) {
            const parenToken = this.previous();
            const expression = this.expression();
            const groupExpression = new GroupExpression(expression);
            this.consume(TokenType.RIGHT_PAREN, "Expect ) after group");
            return this.generateExpression(groupExpression, parenToken);
        }
        else if (this.match(TokenType.IDENTIFIER)) {
            return this.generateExpression(new VarExpression(this.previous()), this.previous());
        }
        else if (this.match(TokenType.NEW)) {
            const newToken = this.previous();
            const name = this.consume(TokenType.IDENTIFIER, "Expect class name after new");
            this.consume(TokenType.LEFT_PAREN, "Expect ( after class name");
            return this.generateExpression(new NewExpression(name, this.arguments()), newToken);
        }
        else if (this.match(TokenType.LEFT_BRACKET)) {
            const arrayToken = this.previous();
            const elements = [];
            while (!this.match(TokenType.RIGHT_BRACKET)) {
                elements.push(this.expression());
                while (this.match(TokenType.COMMA)) {
                    elements.push(this.expression());
                }
            }
            return this.generateExpression(new ArrayExpression(elements), arrayToken);
        }
        else if (this.match(TokenType.DEF)) {
            const defToken = this.previous();
            this.consume(TokenType.LEFT_PAREN, "Expect ( lambda");
            const paramenters = this.parameters();
            this.consume(TokenType.COLON, "Expect : after lambda");
            const returnKind = this.consume(TokenType.IDENTIFIER, "Expect lambda return type");
            this.consume(TokenType.ARROW, "Expect => after lambda");
            return this.generateExpression(new LambdaExpression(paramenters, this.statement(), returnKind.lexeme), defToken);
        }
        return error(this.peek(), "Expect expression");
    }
    arguments() {
        const args = [];
        while (!this.match(TokenType.RIGHT_PAREN)) {
            args.push(this.expression());
            while (this.match(TokenType.COMMA)) {
                args.push(this.expression());
            }
        }
        return args;
    }
    parameters() {
        const paramenters = [];
        while (!this.match(TokenType.RIGHT_PAREN)) {
            paramenters.push(this.parameter());
            while (this.match(TokenType.COMMA)) {
                paramenters.push(this.parameter());
            }
        }
        return paramenters;
    }
    parameter() {
        const name = this.consume(TokenType.IDENTIFIER, "Expect parameter name");
        this.consume(TokenType.COLON, "Expect : after parameter name");
        const kind = this.consume(TokenType.IDENTIFIER, "Expect type after parameter");
        return new ParameterDeclaration(name, kind.lexeme);
    }
    generateStatement(statement, start, end = this.previous()) {
        statement.pStart = { line: start.line, column: start.column };
        statement.pEnd = { line: end.line, column: end.column + end.lexeme.length };
        return statement;
    }
    generateExpression(expression, start, end = this.previous()) {
        expression.pStart = { line: start.line, column: start.column };
        expression.pEnd = {
            line: end.line,
            column: end.column + end.lexeme.length,
        };
        return expression;
    }
    notAtEnd() {
        return (this.current < this.tokens.length && this.peek().type !== TokenType.EOF);
    }
    match(...types) {
        return types.some(type => {
            if (this.check(type)) {
                this.advance();
                return true;
            }
            return false;
        });
    }
    consume(type, errorMessage) {
        if (this.check(type)) {
            return this.advance();
        }
        return error(this.peek(), errorMessage);
    }
    check(type) {
        return this.notAtEnd() && this.peek().type === type;
    }
    advance() {
        return this.tokens[this.current++];
    }
    peek() {
        return this.tokens[this.current];
    }
    previous() {
        return this.tokens[this.current - 1];
    }
    next() {
        return this.tokens[this.current + 1];
    }
}

const MAX_STACK = 500;
class SymbolTable {
    constructor(enclosing) {
        this.enclosing = enclosing;
        this.symbols = {};
    }
    define(name, value) {
        this.symbols[name] = value;
    }
    lookup(name, depth = MAX_STACK) {
        let scope = this;
        if (name) {
            for (let i = 0; i <= depth && scope; ++i) {
                if (Object.keys(scope.symbols).includes(name)) {
                    return scope.symbols[name];
                }
                scope = scope.enclosing;
            }
        }
        throw new Error(`Cannot find ${name}`);
    }
    assign(name, value) {
        let scope = this;
        for (let i = 0; i <= MAX_STACK && scope; ++i) {
            if (Object.keys(scope.symbols).includes(name)) {
                break;
            }
            else {
                scope = scope.enclosing;
            }
        }
        if (scope) {
            scope.symbols[name] = value;
        }
        else {
            throw new Error(`Cannot find ${name}`);
        }
    }
}

class Classable$1 {
    constructor(name) {
        this.name = name;
        this.properties = {};
        this.methods = {};
    }
}

class BreakCall {
}
class ContinueCall {
}
class ReturnCall {
    constructor(value) {
        this.value = value;
    }
}

class Lambda {
    constructor(parameters, body, closure) {
        this.parameters = parameters;
        this.body = body;
        this.closure = closure;
    }
    invoke(interpreter, args) {
        const scope = new SymbolTable(this.closure);
        this.parameters.forEach((parameterName, index) => {
            scope.define(parameterName.name.lexeme, args[index]);
        });
        try {
            interpreter.executeFunctionBody(this.body, scope);
        }
        catch (e) {
            if (e instanceof ReturnCall) {
                return e.value;
            }
            else {
                throw e;
            }
        }
    }
}

class Functionable$1 extends Lambda {
    constructor(parameters, body, closure, name) {
        super(parameters, body, closure);
        this.name = name;
    }
}

class Instance {
    constructor(klass) {
        if (klass.superclass) {
            this.superInstance = new Instance(klass.superclass);
        }
        this.klass = klass;
        this.fields = Object.keys(klass.properties).reduce((acc, iter) => {
            acc[iter] = klass.properties[iter];
            return acc;
        }, {});
        this.methods = Object.keys(klass.methods).reduce((acc, iter) => {
            const klassMethod = klass.methods[iter];
            const scope = new SymbolTable(klassMethod.closure);
            const method = new Functionable$1(klassMethod.parameters, klassMethod.body, scope, klassMethod.name);
            scope.define("this", this);
            if (this.superInstance) {
                scope.define("super", this.superInstance);
            }
            acc[method.name] = method;
            return acc;
        }, {});
    }
    get(name) {
        if (Object.keys(this.fields).includes(name)) {
            return this.fields[name];
        }
        else if (Object.keys(this.methods).includes(name)) {
            return this.methods[name];
        }
        else if (this.superInstance) {
            return this.superInstance.get(name);
        }
        throw new Error(`${this.klass.name} doesn't have ${name}`);
    }
    set(name, value) {
        if (Object.keys(this.fields).includes(name)) {
            this.fields[name] = value;
        }
        else if (this.superInstance) {
            this.superInstance.set(name, value);
        }
        else {
            throw new Error(`${this.klass.name} doesn't have property named ${name}`);
        }
    }
}

class Interpreter {
    constructor(program, scope) {
        this.evaluate = (expression) => {
            return expression ? expression.accept(this) : undefined;
        };
        this.execute = (statement) => {
            return statement ? statement.accept(this) : undefined;
        };
        this.program = program;
        this.scope = scope || new SymbolTable();
    }
    interpret() {
        this.program.statements.forEach(this.execute);
    }
    visitIfStatement(statement) {
        if (this.evaluate(statement.condition)) {
            return this.execute(statement.thenStatement);
        }
        else {
            return this.execute(statement.elseStatement);
        }
    }
    visitBlockStatement(statement) {
        this.beginScope();
        statement.statements.forEach(this.execute);
        this.endScope();
    }
    visitForStatement(statement) {
        this.beginScope();
        if (statement.initializer) {
            const initializers = statement.initializer || [];
            initializers.forEach(initializer => initializer instanceof VarStatement
                ? this.execute(initializer)
                : this.evaluate(initializer));
        }
        while (true) {
            if (!this.evaluate(statement.condition)) {
                break;
            }
            try {
                this.execute(statement.body);
            }
            catch (e) {
                if (e instanceof BreakCall) {
                    break;
                }
                else if (e instanceof ContinueCall) {
                    continue;
                }
                else if (e instanceof ReturnCall) {
                    throw e;
                }
            }
            this.execute(statement.increment);
        }
        this.endScope();
    }
    visitVarStatement(statement) {
        const value = this.evaluate(statement.initializer);
        this.scope.define(statement.name.lexeme, value);
    }
    visitVarsStatements(statement) {
        statement.varStatements.forEach(this.execute);
    }
    visitClassStatement(statement) {
        const klass = new Classable$1(statement.name.lexeme);
        this.scope.define(statement.name.lexeme, klass);
        this.beginScope();
        if (statement.superclass) {
            klass.superclass = this.scope.lookup(statement.superclass.lexeme);
        }
        if (statement.properties) {
            statement.properties.forEach(property => {
                klass.properties[property.name.lexeme] = this.evaluate(property.initializer);
            });
        }
        if (statement.methods) {
            statement.methods.forEach(method => {
                klass.methods[method.name.lexeme] = new Functionable$1(method.parameters, method.body, this.scope, method.name.lexeme);
            });
        }
        this.endScope();
        return klass;
    }
    visitFunctionStatement(statement) {
        const kunction = new Functionable$1(statement.parameters, statement.body, this.scope, statement.name.lexeme);
        this.scope.define(statement.name.lexeme, kunction);
        return kunction;
    }
    visitReturnStatement(statement) {
        const value = this.evaluate(statement.value);
        throw new ReturnCall(value);
    }
    visitBreakStatement(statement) {
        throw new BreakCall();
    }
    visitContinueStatement(statement) {
        throw new ContinueCall();
    }
    visitExpressionStatement(statement) {
        return this.evaluate(statement.expression);
    }
    visitAssignmentExpression(expression) {
        const value = this.evaluate(expression.expression);
        this.scope.assign(expression.name.lexeme, value);
        return value;
    }
    visitLogicalExpression(expression) {
        const left = this.evaluate(expression.left);
        const right = this.evaluate(expression.right);
        switch (expression.operator.type) {
            case TokenType.AND:
                return left && right;
            case TokenType.OR:
                return left || right;
            case TokenType.EQUAL_EQUAL:
                return left === right;
            case TokenType.BANG_EQUAL:
                return left !== right;
            case TokenType.LESS:
                return left < right;
            case TokenType.LESS_THAN:
                return left <= right;
            case TokenType.GREAT:
                return left > right;
            case TokenType.GREAT_THAN:
                return left >= right;
        }
    }
    visitBinaryExpression(expression) {
        const left = this.evaluate(expression.left);
        const right = this.evaluate(expression.right);
        switch (expression.operator.type) {
            case TokenType.PLUS:
                return left + right;
            case TokenType.MINUS:
                return left - right;
            case TokenType.STAR:
                return left * right;
            case TokenType.SLASH:
                return left / right;
            case TokenType.STAR_STAR:
                return left ** right;
        }
    }
    visitUnaryExpression(expression) {
        const value = this.evaluate(expression.right);
        switch (expression.operator.type) {
            case TokenType.BANG:
                return !value;
            case TokenType.PLUS:
                return +value;
            case TokenType.MINUS:
                return -value;
            case TokenType.PLUS_PLUS:
            case TokenType.MINUS_MINUS:
                const newValue = expression.operator.type === TokenType.PLUS_PLUS
                    ? value + 1
                    : value - 1;
                if (expression.right instanceof VarExpression) {
                    this.scope.define(expression.right.name.lexeme, newValue);
                }
                else {
                    error(expression.right.pStart, "has to be variable name");
                }
                return newValue;
        }
    }
    visitCallExpression(expression) {
        const callee = this.evaluate(expression.callee);
        if (callee instanceof Functionable$1 || callee instanceof Lambda) {
            return callee.invoke(this, expression.args.map(this.evaluate));
        }
    }
    visitGetExpression(expression) {
        const instance = this.evaluate(expression.object);
        if (instance instanceof Instance) {
            return instance.get(expression.name.lexeme);
        }
        else {
            error(expression.pStart, "has to be instance");
        }
    }
    visitSetExpression(expression) {
        const instance = this.evaluate(expression.object);
        const value = this.evaluate(expression.value);
        if (instance instanceof Instance) {
            instance.set(expression.name.lexeme, value);
            return value;
        }
        else {
            error(expression.pStart, "has to be instance");
        }
    }
    visitLiteralExpression(expression) {
        return expression.name.literal;
    }
    visitGroupExpression(expression) {
        return this.evaluate(expression.expression);
    }
    visitNewExpression(expression) {
        const klass = this.scope.lookup(expression.name.lexeme);
        if (klass instanceof Classable$1) {
            return new Instance(klass);
        }
        else {
            error(expression.name, "has to be class name");
        }
    }
    visitLambdaExpression(expression) {
        return new Lambda(expression.parameters, expression.body, this.scope);
    }
    visitThisExpression(expression) {
        return this.scope.lookup("this");
    }
    visitSuperExpression(expression) {
        return this.scope.lookup("super");
    }
    visitVarExpression(expression) {
        return this.scope.lookup(expression.name.lexeme);
    }
    visitTupleExpression(expression) { }
    visitArrayExpression(expression) { }
    executeFunctionBody(statement, scope) {
        const currentScope = this.scope;
        try {
            this.scope = scope;
            statement.accept(this);
        }
        finally {
            this.scope = currentScope;
        }
    }
    beginScope() {
        const newScope = new SymbolTable(this.scope);
        const currentScope = this.scope;
        this.scope = newScope;
        return currentScope;
    }
    endScope() {
        if (this.scope.enclosing) {
            this.scope = this.scope.enclosing;
        }
        else {
            throw new Error("There is something wrong with scope");
        }
    }
}

class SymbolTable$1 {
    constructor(enclosing, symbols = {}) {
        this.enclosing = enclosing;
        this.symbols = symbols;
    }
    define(name, value) {
        this.symbols = { ...this.symbols, [name]: value };
    }
    lookup(name, depth = Number.MAX_SAFE_INTEGER) {
        let scope = this;
        if (name) {
            for (let i = 0; i <= depth && scope; ++i) {
                if (Object.keys(scope.symbols).includes(name)) {
                    return scope.symbols[name];
                }
                scope = scope.enclosing;
            }
        }
        throw new Error(`Cannot find ${name}`);
    }
}

class TypeChecking {
    constructor(program, scope) {
        this.program = program;
        this.scope = scope || new SymbolTable$1(undefined, BuiltinTypes);
    }
    run() {
        this.program.statements.forEach(statement => this.evaluateStatement(statement));
    }
    visitIfStatement(statement) {
        this.evaluateExpression(statement.condition);
        this.evaluateStatement(statement.thenStatement);
        this.evaluateStatement(statement.elseStatement);
    }
    visitBlockStatement(statement) {
        this.beginScope();
        statement.statements.forEach(statement => this.evaluateStatement(statement));
        this.endScope();
    }
    visitForStatement(statement) {
        this.beginScope();
        const initializers = statement.initializer || [];
        initializers.forEach(declaration => declaration instanceof VarStatement
            ? this.evaluateStatement(declaration)
            : this.evaluateExpression(declaration));
        this.evaluateExpression(statement.condition);
        this.beginScope();
        this.evaluateStatement(statement.body);
        this.endScope();
        this.evaluateStatement(statement.increment);
        this.endScope();
    }
    visitVarStatement(statement) {
        this.checkVarStatement(statement, (name, type) => this.scope.define(name, type));
    }
    visitVarsStatements(statement) {
        statement.varStatements.forEach(varStatement => this.visitVarStatement(varStatement));
    }
    visitClassStatement(statement) {
        const klass = new Classable(statement.name.lexeme);
        this.scope.define(statement.name.lexeme, klass);
        this.beginScope();
        this.scope.define("this", klass);
        if (statement.superclass) {
            const superType = this.scope.lookup(statement.superclass.lexeme);
            if (superType instanceof Classable) {
                klass.superclass = superType;
                this.scope.define("super", klass.superclass);
            }
            else {
                error(statement.superclass, "super has to be classname");
            }
        }
        if (statement.properties) {
            statement.properties.forEach(property => {
                this.checkVarStatement(property, (name, type) => (klass.properties[name] = type));
            });
        }
        if (statement.methods) {
            statement.methods.forEach(method => {
                this.checkFunctionStatement(method, (name, type) => (klass.methods[name] = type));
            });
        }
        this.endScope();
    }
    visitFunctionStatement(statement) {
        this.checkFunctionStatement(statement, (name, type) => this.scope.define(name, type));
    }
    visitExpressionStatement(statement) {
        this.evaluateExpression(statement.expression);
    }
    visitReturnStatement(statement) {
        const kunction = this.scope.lookup("this");
        if (kunction instanceof Functionable) {
            const type = this.evaluateExpression(statement.value);
            checkInheritance(type, kunction.returnType, statement.pStart);
        }
        else {
            error(statement.pStart, "cannot return without function's scope");
        }
    }
    visitAssignmentExpression(expression) {
        const expressionType = this.evaluateExpression(expression.expression);
        const variableType = this.scope.lookup(expression.name.lexeme);
        checkInheritance(expressionType, variableType, expression.pStart);
        return variableType;
    }
    visitLogicalExpression(expression) {
        return expression.type;
    }
    visitBinaryExpression(expression) {
        const leftType = this.evaluateExpression(expression.left);
        const rightType = this.evaluateExpression(expression.right);
        if (leftType === BuiltinTypes.String &&
            rightType === BuiltinTypes.String &&
            expression.operator.type === TokenType.PLUS) {
            return (expression.type = BuiltinTypes.String);
        }
        else if (leftType === BuiltinTypes.Number &&
            rightType === BuiltinTypes.Number) {
            return (expression.type = BuiltinTypes.Number);
        }
        else {
            error(expression.operator, "Only can do binary operator for number and string type");
        }
    }
    visitUnaryExpression(expression) {
        this.evaluateExpression(expression.right);
        switch (expression.operator.type) {
            case TokenType.BANG:
                return (expression.type = BuiltinTypes.Boolean);
            case TokenType.PLUS:
            case TokenType.PLUS_PLUS:
            case TokenType.MINUS:
            case TokenType.MINUS_MINUS:
                return (expression.type = BuiltinTypes.Number);
            default:
                error(expression.operator, "Unknow operator");
        }
    }
    visitCallExpression(expression) {
        const type = this.evaluateExpression(expression.callee);
        if (type instanceof Functionable) {
            type.parameters.forEach((parameterType, index) => {
                const argType = this.evaluateExpression(expression.args[index]);
                checkInheritance(argType, parameterType, expression.pStart);
            });
            return type.returnType;
        }
        else {
            error(expression.callee.pStart, "is not callable");
        }
    }
    visitGetExpression(expression) {
        const type = this.evaluateExpression(expression.object);
        return type.get(expression.name.lexeme);
    }
    visitSetExpression(expression) {
        const object = this.evaluateExpression(expression.object);
        if (object instanceof Classable) {
            const type = object.get(expression.name.lexeme);
            const setType = this.evaluateExpression(expression.value);
            checkInheritance(setType, type, expression.name);
            return type;
        }
        else {
            error(expression.name, "cannot set");
        }
    }
    visitLiteralExpression(expression) {
        return expression.type;
    }
    visitGroupExpression(expression) {
        this.evaluateExpression(expression.expression);
        return (expression.type = expression.expression.type);
    }
    visitLambdaExpression(expression) {
        const kunction = new Functionable(this.scope.lookup(expression.returnType));
        expression.parameters.forEach(parameter => {
            const parameterType = this.scope.lookup(parameter.type);
            kunction.parameters.push(parameterType);
            this.scope.define(parameter.name.lexeme, parameterType);
        });
        this.beginScope();
        this.scope.define("this", kunction);
        this.evaluateStatement(expression.body);
        this.endScope();
        return kunction;
    }
    visitNewExpression(expression) {
        return this.scope.lookup(expression.name.lexeme);
    }
    visitThisExpression(expression) {
        return this.scope.lookup(expression.keyword.lexeme);
    }
    visitSuperExpression(expression) {
        return this.scope.lookup(expression.keyword.lexeme);
    }
    visitVarExpression(expression) {
        debugger;
        return this.scope.lookup(expression.name.lexeme);
    }
    visitArrayExpression(expression) { }
    visitTupleExpression(expression) { }
    visitBreakStatement(statement) { }
    visitContinueStatement(statement) { }
    evaluateStatement(statement) {
        statement && statement.accept(this);
    }
    evaluateExpression(expression) {
        return expression && expression.accept(this);
    }
    beginScope() {
        const enclosing = this.scope;
        this.scope = new SymbolTable$1(enclosing);
        return enclosing;
    }
    endScope() {
        if (this.scope.enclosing) {
            this.scope = this.scope.enclosing;
        }
    }
    checkVarStatement(statement, define) {
        const initializerType = this.evaluateExpression(statement.initializer);
        if (statement.type) {
            const variableType = this.scope.lookup(statement.type);
            checkInheritance(initializerType, variableType, statement.name);
            define(statement.name.lexeme, variableType);
        }
        else if (initializerType) {
            define(statement.name.lexeme, initializerType);
        }
        else {
            error(statement.name, "cannot declare without type");
        }
    }
    checkFunctionStatement(statement, define) {
        const kunction = new Functionable(this.scope.lookup(statement.returnType), statement.name.lexeme);
        statement.parameters.forEach(parameter => {
            const parameterType = this.scope.lookup(parameter.type);
            kunction.parameters.push(parameterType);
            this.scope.define(parameter.name.lexeme, parameterType);
        });
        define(statement.name.lexeme, kunction);
        this.beginScope();
        this.scope.define("this", kunction);
        this.evaluateStatement(statement.body);
        this.endScope();
    }
}

class Repl {
    constructor() {
        this.scope = new SymbolTable();
        this.scopeTypes = new SymbolTable$1(undefined, BuiltinTypes);
    }
    execute() {
        while (true) {
            const tokens = new Lexer(this.getInput()).scan();
            const ast = new Parser(tokens).parse();
            new TypeChecking(ast, this.scopeTypes).run();
            const interpreter = new Interpreter(ast, this.scope);
            const result = ast.statements.reduce((_, statement) => interpreter.execute(statement), undefined);
            console.log(result);
        }
    }
    getInput() {
        return readlineSync_6("> ");
    }
}
new Repl().execute();

exports.Repl = Repl;
