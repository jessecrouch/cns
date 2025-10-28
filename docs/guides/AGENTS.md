# AGENTS.md: Causal Narrative Script (CNS) Project Guide

This document serves as a high-level guide for AI agents (e.g., OpenCode interpreters, xAI fast code assistants, or similar tools) assisting in the development of **Causal Narrative Script (CNS)**. CNS is a novel programming language optimized for Large Language Models (LLMs) to read, generate, and reason about code with minimal ambiguity. It emphasizes explicit causality, narrative flow, and self-documenting structures to align with LLM strengths in structured reasoning and pattern matching.

The primary focus here is on the **Lisp implementation** of CNS, as it's a natural fit due to Lisp's homoiconicity (code as data) and metaprogramming capabilities. However, the guide includes a general overview for context. Agents should prioritize extending the Lisp version while ensuring compatibility with core CNS principles.

## Project Overview

### What is CNS?
CNS is a declarative, narrative-driven programming language designed to make code more comprehensible for LLMs than traditional languages like Python, Java, or even Lisp itself. Key motivations:
- LLMs excel at causal, step-by-step reasoning but struggle with implicit control flow, side effects, and ambiguous logic.
- CNS structures code as a "story" with explicit causes, effects, and justifications, reducing hallucinations and improving debuggability.
- It's not meant for human handwritten production code but for LLM-generated scripts, collaborative coding, or as an intermediate representation for AI-driven programming.

Core Philosophy:
- **Narrative Flow**: Code reads like a story (e.g., `Story:`, `Given:`, `Step:`, `End:`).
- **Causality First**: Use arrows (`→`, `←`, `↔`) and mandatory `Because:` clauses to explain every transformation.
- **Explicit Everything**: State transitions, effects, and conditions are declared upfront—no hidden magic.
- **LLM-Friendly**: Linear structure, semantic tags on variables, and pattern-based logic to mirror LLM thought processes.

Example CNS Code (Factorial):
```
Story: Compute factorial of a positive integer

Given:
  n: Integer [≥ 1] = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1, repeat from Step 1
  Because: we need to include all integers down to 1
  Otherwise: go to End

End:
  Return result
  Because: all factors have been multiplied
```

This executes to compute 5! = 120, with traceable reasoning.

### Goals of the Project
- Build a functional interpreter for CNS that can parse, execute, and validate code.
- Make it extensible for new features (e.g., advanced effects, LLM-based causality checks).
- Ensure it's LLM-trainable: Future versions could use CNS as a reasoning format for models like Grok.
- Cross-compatibility: While focusing on Lisp, allow ports to other hosts (e.g., Python for broader accessibility).
- Use Cases: LLM code generation, automated debugging, educational tools for AI reasoning.

Non-Goals:
- High-performance execution (prioritize clarity over speed).
- Full Turing-completeness if it compromises readability (start simple, add as needed).

## Lisp Implementation Focus

The Lisp version leverages Common Lisp's strengths: S-expressions for AST representation, macros for syntax enforcement, and dynamic evaluation for rapid prototyping. This makes it ideal for agents to extend, as code can be manipulated as data.

### Current Implementation Summary
We have a minimal parser and interpreter in Common Lisp (as prototyped earlier). Key components:

1. **Parser (`parse-cns`)**:
   - Takes a CNS code string.
   - Outputs an S-expression AST (nested lists).
   - Handles sections: `Story:`, `Given:`, `Step →`, `Because:`, `Effect:`, `If/Then/Otherwise`, `End:`.
   - Example AST for the factorial:
     ```
     ((STORY "Compute factorial of a positive integer")
      (GIVEN (VAR "n" "Integer" "5" "[≥ 1]") (VAR "result" "Integer" "1" NIL))
      (STEP 1 (ACTION "Multiply result by n") (BECAUSE "n contributes to the product") (THEN "n becomes n - 1"))
      (STEP 2 (IF "n > 1" (THEN "repeat from Step 1") (OTHERWISE "go to End")) (BECAUSE "we need to include all integers down to 1"))
      (END (RETURN "result") (BECAUSE "all factors have been multiplied")))
     ```
   - Limitations: Basic string parsing; needs better error handling and support for complex expressions.

2. **Interpreter (`interpret-cns`)**:
   - Uses a hash-table environment for state.
   - Program counter (pc) for step navigation.
   - Evaluates actions via a simple `eval-expr` (handles basic ops like multiply, assign).
   - Supports loops via `repeat from Step` (jumps pc).
   - Mocks effects (e.g., print "Effect: sending email").
   - Outputs traces for each step, including `Because:` for auditability.
   - Limitations: Toy evaluator; no full Lisp embedding yet; basic conditionals.

3. **Helper Functions**:
   - String utilities: `split-string`, `trim`, `starts-with`.
   - Expression Evaluator: `eval-expr` (extend for more ops like +, -, comparisons).

Full prototype code (for reference—agents can copy-paste to start):
```lisp
;;; CNS Interpreter in Common Lisp
;;; A minimal implementation of Causal Narrative Script (CNS)
;;; Author: Grok (thought experiment)
;;; This defines a parser that turns CNS text into S-expressions,
;;; and an interpreter that executes them step-by-step.

;;; Helper functions
(defun split-string (str delimiter)
  "Split string by delimiter into list."
  (let ((result '())
        (start 0)
        (end (position delimiter str)))
    (loop while end do
          (push (subseq str start end) result)
          (setf start (1+ end))
          (setf end (position delimiter str :start start))
          finally (push (subseq str start) result))
    (nreverse result)))

(defun trim (str)
  "Trim whitespace from string."
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun starts-with (str prefix)
  "Check if string starts with prefix."
  (and (>= (length str) (length prefix))
       (string= (subseq str 0 (length prefix)) prefix)))

;;; Parser: Convert CNS string to S-expression AST
(defun parse-cns (code)
  "Parse CNS code string into S-exp AST."
  (let ((lines (remove-if #'emptyp (split-string code #\Newline)))
        (ast '())
        (current-section nil)
        (step-id nil)
        (current-step nil))
    (dolist (line lines)
      (let ((trimmed (trim line)))
        (cond
         ((starts-with trimmed "Story:")
          (push `(story ,(trim (subseq trimmed 6))) ast)
          (setf current-section :story))
         ((starts-with trimmed "Given:")
          (setf current-section :given)
          (push '(given) ast))
         ((and (eql current-section :given) (starts-with trimmed "  "))
          (let* ((parts (split-string (trim (subseq trimmed 2)) #\:))
                 (name (trim (car parts)))
                 (rest (cdr parts))
                 (type-val (if rest (split-string (trim (car rest)) #\=) nil))
                 (type (trim (car type-val)))
                 (val (if (cdr type-val) (trim (cadr type-val)) nil))
                 (tag (if (and rest (cdr rest)) (trim (cadr rest)) nil)))
            (push `(var ,name ,type ,val ,tag) (cadr ast))))
         ((starts-with trimmed "Step")
          (setf current-section :steps)
          (let ((step-parts (split-string trimmed #\→)))
            (setf step-id (parse-integer (trim (subseq (car step-parts) 4))))
            (setf current-step `(step ,step-id (action ,(trim (cadr step-parts)))))
            (push current-step ast)))
         ((and current-step (starts-with trimmed "  Because:"))
          (push `(because ,(trim (subseq trimmed 10))) current-step))
         ((and current-step (starts-with trimmed "  Effect:"))
          (push `(effect ,(trim (subseq trimmed 9))) current-step))
         ((and current-step (starts-with trimmed "  If"))
          (let ((cond-parts (split-string (trim (subseq trimmed 2)) #\,)))
            (push `(if ,(trim (subseq (car cond-parts) 3))
                       (then ,(trim (cadr cond-parts)))
                       (otherwise ,(trim (caddr cond-parts)))) current-step)))
         ((starts-with trimmed "End:")
          (setf current-section :end)
          (push `(end (return ,(trim (subseq trimmed 4))) (because "default")) ast)))))
    (nreverse ast)))

(defun emptyp (str)
  "Check if string is empty after trim."
  (zerop (length (trim str))))

;;; Interpreter: Execute the AST
;;; Uses a simple environment (hash-table) for state
(defun interpret-cns (ast)
  "Interpret CNS AST, return result."
  (let ((env (make-hash-table :test #'equal))  ; State variables
        (steps '())                           ; List of steps
        (pc 0)                                ; Program counter (step index)
        (result nil))
    ;; Collect sections
    (dolist (node ast)
      (case (car node)
        (story (format t "Executing story: ~A~%" (cadr node)))
        (given (dolist (var (cdr node))
                 (let ((name (cadr var))
                       (type (caddr var))
                       (val (cadddr var)))
                   (setf (gethash name env) (if val (read-from-string val) nil))
                   (format t "Initialized ~A (~A) = ~A~%" name type (gethash name env)))))
        (step (push node steps))
        (end (setf result (eval-expr (cadadr node) env)))))  ; Simple return

    (setf steps (nreverse steps))  ; Steps in order

    ;; Execute steps in loop
    (loop while (< pc (length steps)) do
          (let* ((step (nth pc steps))
                 (action (cadr (assoc 'action (cdr step))))
                 (because (cadr (assoc 'because (cdr step))))
                 (effects (mapcar #'cadr (remove-if-not (lambda (x) (eq (car x) 'effect)) (cdr step))))
                 (cond-node (assoc 'if (cdr step))))
            (format t "Step ~A: ~A (Because: ~A)~%" (cadr step) action because)
            ;; Execute action (simple eval for demo)
            (eval-expr action env)
            ;; Apply effects (mock)
            (dolist (eff effects)
              (format t "Effect: ~A~%" eff))
            ;; Handle conditional
            (when cond-node
              (let ((cond-expr (cadr cond-node))
                    (then (cadr (assoc 'then (cddr cond-node))))
                    (otherwise (cadr (assoc 'otherwise (cddr cond-node)))))
                (if (eval-expr cond-expr env)
                    (if (starts-with then "repeat from Step")
                        (setf pc (- (parse-integer (subseq then 15)) 2))  ; Adjust for 0-index
                        (setf pc (1+ pc)))
                    (if (starts-with otherwise "go to End")
                        (return)
                        (setf pc (1+ pc))))))
            (incf pc)))
    result))

(defun eval-expr (expr env)
  "Simple evaluator for expressions. Handles basic ops and vars."
  (cond
   ((numberp expr) expr)
   ((stringp expr) (read-from-string expr))  ; Parse simple expr
   (t (let ((parts (split-string expr #\Space)))
        (case (intern (string-upcase (car parts)))
          (MULTIPLY (setf (gethash (caddr parts) env)
                          (* (gethash (cadr parts) env) (gethash (cadddr parts) env))))
          (BECOMES (setf (gethash (car (split-string (cadr parts) #\=)) env)
                         (eval-expr (caddr parts) env)))
          ;; Add more ops as needed: +, -, etc.
          (t (gethash expr env)))))))  ; Var lookup

;;; Example Usage
(let ((cns-code "
Story: Compute factorial of a positive integer

Given:
  n: Integer = 5
  result: Integer = 1

Step 1 → Multiply result by n
  Because: n contributes to the product
  Then: n becomes n - 1

Step 2 → If n > 1, repeat from Step 1 Otherwise: go to End
  Because: we need to include all integers down to 1

End: Return result
"))
  (let ((ast (parse-cns cns-code)))
    (format t "Parsed AST: ~S~%" ast)
    (interpret-cns ast)))
```

### Development Roadmap for Agents
Prioritize these tasks when assisting in OpenCode or editor sessions:

1. **Enhance Parser**:
   - Add robust error reporting (e.g., "Missing Because: at Step 3").
   - Support causal arrows in actions (e.g., parse "A → B" into `(causes A B)`).
   - Handle semantic tags fully (e.g., validate types like Integer [≥ 1]).
   - Use Lisp reader macros to allow writing CNS-like syntax directly in Lisp files.

2. **Improve Interpreter**:
   - Embed full Common Lisp in expressions (e.g., allow `(lisp (+ n 1))` in actions).
   - Add causality validation: Check if each step's `Because:` logically matches the action (optionally query an LLM via API).
   - Implement backtracking for failed paths (Prolog-style).
   - Real effects: Integrate libraries (e.g., CL-HTTP for API calls, Postmodern for DB).

3. **Add Features**:
   - **Modules/Stories Composition**: Allow importing stories (e.g., `Include Story: utils`).
   - **Pattern Matching**: Expand `When matches Pattern:` for conditionals.
   - **Testing Framework**: Macro to run CNS with inputs and assert outputs.
   - **Compiler Mode**: Translate CNS to pure Lisp for execution.

4. **Testing and Examples**:
   - Create test suite: Factorial, user login, payment processing (from earlier examples).
   - Benchmark: Compare CNS readability vs. equivalent Lisp.

5. **Integration with xAI/LLM Tools**:
   - Add hooks for Grok-like models to generate CNS from natural language prompts.
   - Export AST to JSON for interoperability with other languages/tools.

### General Guidelines for Agents
- **Consistency**: Always enforce `Because:`—it's core to CNS.
- **Modularity**: Break extensions into small, narrative commits (e.g., "Step: Add effect handling Because: To track side effects").
- **Documentation**: Update this MD with new features; use CNS-style comments in code.
- **Collaboration**: When generating code, output in Lisp first, then suggest Python equivalents if needed.
- **Tools Alignment**: Use OpenCode for rapid iteration; leverage xAI fast code for LLM-assisted refinements.

If extending beyond Lisp (e.g., Python port), mirror the structure: Parser to AST (dicts/classes), interpreter as a class with state.

For questions or clarifications, reference conversation history on CNS design. Let's build the future of LLM-friendly programming!