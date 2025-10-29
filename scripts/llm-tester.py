#!/usr/bin/env python3
"""
LLM Test Harness for CNS Code Generation
Tests language models' ability to generate valid CNS code
"""

import argparse
import json
import os
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, List, Tuple

try:
    import requests
except ImportError:
    print("ERROR: requests library not found. Install with: pip install requests")
    sys.exit(1)


class GrokClient:
    """Client for xAI's Grok API"""
    
    def __init__(self, api_key: str, model: str = "grok-2-latest"):
        self.api_key = api_key
        self.model = model
        self.base_url = "https://api.x.ai/v1"
        
    def generate(self, prompt: str, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        """Generate code from prompt using Grok API"""
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json"
        }
        
        payload = {
            "messages": [
                {
                    "role": "system",
                    "content": "You are a CNS code generator. Generate ONLY valid CNS code. Do not include explanations, markdown formatting, or any text outside the CNS code block."
                },
                {
                    "role": "user",
                    "content": prompt
                }
            ],
            "model": self.model,
            "temperature": temperature,
            "max_tokens": max_tokens
        }
        
        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=payload,
            timeout=30
        )
        
        if response.status_code != 200:
            raise Exception(f"API Error {response.status_code}: {response.text}")
        
        result = response.json()
        return result["choices"][0]["message"]["content"]


class CNSTester:
    """Tests CNS code generation and validation"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.validator_path = project_root / "src" / "cns-validate"
        self.runner_path = project_root / "src" / "cns-run"
        self.results_dir = project_root / "tests" / "llm-tests" / "results"
        self.generated_dir = project_root / "tests" / "llm-tests" / "generated"
        
        # Create directories
        self.results_dir.mkdir(parents=True, exist_ok=True)
        self.generated_dir.mkdir(parents=True, exist_ok=True)
        
    def extract_cns_code(self, llm_output: str) -> str:
        """Extract CNS code from LLM output, removing markdown/explanations"""
        # Try to find code between ```cns and ```
        if "```cns" in llm_output:
            start = llm_output.find("```cns") + 6
            end = llm_output.find("```", start)
            if end != -1:
                return llm_output[start:end].strip()
        
        # Try generic code blocks
        if "```" in llm_output:
            start = llm_output.find("```") + 3
            # Skip language identifier if present
            newline = llm_output.find("\n", start)
            if newline != -1:
                start = newline + 1
            end = llm_output.find("```", start)
            if end != -1:
                return llm_output[start:end].strip()
        
        # Return as-is if no code blocks found
        return llm_output.strip()
    
    def validate_cns(self, code: str) -> Tuple[bool, str]:
        """Validate CNS code using cns-validate"""
        # Write to temp file
        temp_file = self.generated_dir / "temp_validate.cns"
        temp_file.write_text(code)
        
        try:
            result = subprocess.run(
                [str(self.validator_path), str(temp_file)],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            success = result.returncode == 0
            output = result.stdout + result.stderr
            
            return success, output
        except subprocess.TimeoutExpired:
            return False, "Validation timeout"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
    
    def execute_cns(self, code: str, timeout: int = 5) -> Tuple[bool, str]:
        """Execute CNS code using cns-run"""
        temp_file = self.generated_dir / "temp_execute.cns"
        temp_file.write_text(code)
        
        try:
            result = subprocess.run(
                [str(self.runner_path), str(temp_file)],
                capture_output=True,
                text=True,
                timeout=timeout
            )
            
            success = result.returncode == 0
            output = result.stdout + result.stderr
            
            return success, output
        except subprocess.TimeoutExpired:
            return False, "Execution timeout"
        except Exception as e:
            return False, f"Execution error: {str(e)}"
    
    def save_result(self, test_name: str, result_data: Dict) -> Path:
        """Save test results to JSON file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{test_name}_{timestamp}.json"
        filepath = self.results_dir / filename
        
        with open(filepath, 'w') as f:
            json.dump(result_data, indent=2, fp=f)
        
        return filepath
    
    def save_generated_code(self, test_name: str, iteration: int, code: str) -> Path:
        """Save generated CNS code"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{test_name}_iter{iteration}_{timestamp}.cns"
        filepath = self.generated_dir / filename
        filepath.write_text(code)
        return filepath


def load_prompt_template(template_path: Path, task: str) -> str:
    """Load and fill prompt template"""
    if not template_path.exists():
        raise FileNotFoundError(f"Template not found: {template_path}")
    
    template = template_path.read_text()
    return template.replace("{TASK}", task)


def run_test(
    grok: GrokClient,
    tester: CNSTester,
    prompt: str,
    test_name: str,
    max_retries: int = 3,
    verbose: bool = True
) -> Dict:
    """Run a single test with optional retries"""
    
    result = {
        "test_name": test_name,
        "model": grok.model,
        "timestamp": datetime.now().isoformat(),
        "prompt": prompt,
        "attempts": [],
        "success": False,
        "total_attempts": 0
    }
    
    for attempt in range(1, max_retries + 1):
        if verbose:
            print(f"\n{'='*60}")
            print(f"Attempt {attempt}/{max_retries}")
            print(f"{'='*60}")
        
        attempt_data = {
            "attempt_number": attempt,
            "validation_passed": False,
            "execution_passed": False
        }
        
        try:
            # Generate code
            if verbose:
                print("Generating code...")
            
            start_time = time.time()
            llm_output = grok.generate(prompt)
            generation_time = time.time() - start_time
            
            attempt_data["generation_time"] = generation_time
            attempt_data["raw_output"] = llm_output
            
            # Extract CNS code
            cns_code = tester.extract_cns_code(llm_output)
            attempt_data["cns_code"] = cns_code
            
            if verbose:
                print(f"Generated in {generation_time:.2f}s")
                print(f"\nCode length: {len(cns_code)} chars")
            
            # Save generated code
            code_path = tester.save_generated_code(test_name, attempt, cns_code)
            attempt_data["code_file"] = str(code_path)
            
            # Validate
            if verbose:
                print("Validating...")
            
            valid, validation_output = tester.validate_cns(cns_code)
            attempt_data["validation_passed"] = valid
            attempt_data["validation_output"] = validation_output
            
            if verbose:
                if valid:
                    print("✓ VALIDATION PASSED")
                else:
                    print("✗ VALIDATION FAILED")
                    print(f"  {validation_output[:200]}")
            
            if not valid:
                # Add validation error to prompt for retry
                prompt += f"\n\nPrevious attempt failed validation with error:\n{validation_output}\n\nPlease fix and try again."
                result["attempts"].append(attempt_data)
                continue
            
            # Execute
            if verbose:
                print("Executing...")
            
            executed, execution_output = tester.execute_cns(cns_code)
            attempt_data["execution_passed"] = executed
            attempt_data["execution_output"] = execution_output
            
            if verbose:
                if executed:
                    print("✓ EXECUTION PASSED")
                    print(f"\nOutput:\n{execution_output}")
                else:
                    print("✗ EXECUTION FAILED")
                    print(f"  {execution_output[:200]}")
            
            result["attempts"].append(attempt_data)
            
            if valid and executed:
                result["success"] = True
                result["total_attempts"] = attempt
                if verbose:
                    print(f"\n🎉 SUCCESS on attempt {attempt}!")
                break
            
            # Add execution error to prompt for retry
            if not executed:
                prompt += f"\n\nPrevious attempt passed validation but failed execution:\n{execution_output}\n\nPlease fix and try again."
        
        except Exception as e:
            attempt_data["error"] = str(e)
            result["attempts"].append(attempt_data)
            if verbose:
                print(f"✗ ERROR: {e}")
    
    result["total_attempts"] = len(result["attempts"])
    
    # Save results
    result_path = tester.save_result(test_name, result)
    if verbose:
        print(f"\n{'='*60}")
        print(f"Results saved to: {result_path}")
        print(f"{'='*60}")
    
    return result


def load_env_file(project_root: Path):
    """Load environment variables from .env file"""
    env_file = project_root / ".env"
    if env_file.exists():
        with open(env_file) as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    os.environ.setdefault(key.strip(), value.strip())


def main():
    parser = argparse.ArgumentParser(description="Test LLMs on CNS code generation")
    parser.add_argument("--task", required=True, help="Task description (e.g., 'factorial function')")
    parser.add_argument("--name", help="Test name (defaults to task)")
    parser.add_argument("--template", default="prompts/quick-template.md", help="Prompt template file")
    parser.add_argument("--model", default="grok-2-latest", help="Grok model to use (grok-2-latest, grok-3, etc.)")
    parser.add_argument("--retries", type=int, default=3, help="Max retry attempts")
    parser.add_argument("--api-key", help="xAI API key (or set GROK_API_KEY in .env file)")
    parser.add_argument("--quiet", action="store_true", help="Minimal output")
    
    args = parser.parse_args()
    
    # Setup project root and load .env
    project_root = Path(__file__).parent.parent
    load_env_file(project_root)
    
    # Get API key from args, GROK_API_KEY env var, or XAI_API_KEY env var
    api_key = args.api_key or os.environ.get("GROK_API_KEY") or os.environ.get("XAI_API_KEY")
    if not api_key:
        print("ERROR: No API key provided.")
        print("  Option 1: Add GROK_API_KEY=your-key to .env file")
        print("  Option 2: Set GROK_API_KEY or XAI_API_KEY environment variable")
        print("  Option 3: Use --api-key command line argument")
        sys.exit(1)
    
    # Setup
    project_root = Path(__file__).parent.parent
    load_env_file(project_root)
    test_name = args.name or args.task.replace(" ", "-").lower()
    
    # Load prompt
    template_path = project_root / args.template
    try:
        prompt = load_prompt_template(template_path, args.task)
    except FileNotFoundError as e:
        print(f"ERROR: {e}")
        sys.exit(1)
    
    # Initialize clients
    grok = GrokClient(api_key, model=args.model)
    tester = CNSTester(project_root)
    
    print(f"\n{'='*60}")
    print(f"CNS LLM Test Harness")
    print(f"{'='*60}")
    print(f"Task: {args.task}")
    print(f"Model: {args.model}")
    print(f"Max retries: {args.retries}")
    print(f"Template: {args.template}")
    
    # Run test
    result = run_test(
        grok=grok,
        tester=tester,
        prompt=prompt,
        test_name=test_name,
        max_retries=args.retries,
        verbose=not args.quiet
    )
    
    # Summary
    print(f"\n{'='*60}")
    print("SUMMARY")
    print(f"{'='*60}")
    print(f"Success: {result['success']}")
    print(f"Attempts: {result['total_attempts']}")
    
    if result['success']:
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
