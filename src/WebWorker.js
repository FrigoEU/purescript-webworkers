"use strict";

// module WebWorker
/* global exports, Worker, window, postMessage, onmessage */

exports.supportsWebWorkers = !!Worker;

exports.mkWorker = function mkWorker(path){
  return function(handler){
    return function(){
      var w = new Worker(path);
      w.onmessage = handler;
      return new Worker(path);
    };
  };
};

exports.postMessageToWW = function postMessageToWW(worker){
  return function(data){
    return function(){
      worker.postMessage(data);
    };
  };
};

exports.postMessage = postMessage;
exports.onmessage = onmessage;
