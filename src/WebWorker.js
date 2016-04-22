"use strict";

// module WebWorker
/* global exports, Worker, window, postMessage, onmessage */

exports.supportsWebWorkers = !!Worker;

exports.mkWorker = function mkWorker(path){
  return function(){
    return new Worker(path);
  };
};
exports.onmessageFromWorker = function(worker){
  return function(handler){
    return function(){
      worker.onmessage = handler;
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
