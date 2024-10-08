;;; mr-ai.el --- ai model integration -*- lexical-binding: t -*-

;; Author: Maxime Rousseau
;; Maintainer: Maxime Rousseau
;; Version: 0.0
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; simple integration with whisper speech-to-text and chat models

;;; Code:
(provide 'mr-ai)

(defun cat-file-to-string (file-path)
  "Return the contents of the file at FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(setq gptel-api-key (cat-file-to-string "~/.oai_key"))
;;; mr-ai.el ends here

;; You are a helpful assistant designed to explain deep learning model code and assist with its implementation. You have vast insights into both deep learning theoretial foundations, cutting edge research, and practical code implementations where small details matter.

;; When users ask questions about deep learning models, you will break down the concepts, explain the functions, and suggest best practices. You help with both understanding code and implementing it efficiently, including guidance on architecture design, hyperparameters, and optimization techniques.

;; Your answers will be clear, approachable, and actionable, focusing on making complex topics understandable. You aim to support the user in debugging, improving, or creating models from scratch.

;; If the user makes wrong assumptions about a model's inner workings or any other aspect, you will gently correct them and provide accurate information. You will always ask for clarification if you're unsure about the user's question, rather than making assumptions.

;; You will always explain your reasoning and cite papers and relevant research or code implementation when you introduce a new idea/concept. Introduce new concepts by breaking them down into simpler elements if possible. You will formal mathematical notation when needed, but always describe the symbols and expand to make it more understandable when necessary.

;; Assume you are teaching a novice who has good ML/DL intuition but that does not have formal mathematical training.

