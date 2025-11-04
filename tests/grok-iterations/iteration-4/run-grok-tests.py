#!/usr/bin/env python3
"""
Run Grok Iteration 4 Tests
Submits pre-built full prompts to Grok and saves results
"""

import json
import os
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

try:
    import requests
except ImportError:
    print("ERROR: requests library not found. Install with: pip install requests")
    sys.exit(1)


class GrokTester:
    def __init__(self, api_key: str, model: str = "grok-2-latest"):
        self.api_key = api_key
        self.model = model
        self.base_url = "https://api.x.ai/v1"
        
    def generate(self, prompt: str, max_tokens: int = 4000, temperature: float = 0.7) -> str:
        """Call Grok API with prompt"""
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json"
        }
        
        messages = [{"role": "user", "content": prompt}]
        
        payload = {
            "messages": messages,
            "model": self.model,
            "temperature": temperature,
            "max_tokens": max_tokens
        }
        
        print(f"Calling Grok API ({self.model})...")
        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=payload,
            timeout=120
        )
        
        if response.status_code != 200:
            raise Exception(f"API Error {response.status_code}: {response.text}")
        
        result = response.json()
        return result["choices"][0]["message"]["content"]
    
    def extract_cns_code(self, llm_output: str) -> str:
        """Extract CNS code from LLM output"""
        # Try to find code between ```cns or ```cnsc and ```
        for marker in ["```cnsc", "```cns"]:
            if marker in llm_output:
                start = llm_output.find(marker) + len(marker)
                newline = llm_output.find("\n", start)
                if newline != -1:
                    start = newline + 1
                end = llm_output.find("```", start)
                if end != -1:
                    return llm_output[start:end].strip()
        
        # Try generic code blocks
        if "```" in llm_output:
            start = llm_output.find("```") + 3
            newline = llm_output.find("\n", start)
            if newline != -1:
                start = newline + 1
            end = llm_output.find("```", start)
            if end != -1:
                return llm_output[start:end].strip()
        
        # Return as-is if no code blocks found
        return llm_output.strip()
    
    def validate_cns(self, code: str, validator_path: Path) -> tuple:
        """Validate CNS code"""
        temp_file = Path("/tmp/grok_test_validate.cns")
        if not code.endswith('\n'):
            code += '\n'
        temp_file.write_text(code)
        
        try:
            result = subprocess.run(
                [str(validator_path), str(temp_file)],
                capture_output=True,
                text=True,
                timeout=10
            )
            success = result.returncode == 0
            output = result.stdout + result.stderr
            return success, output
        except Exception as e:
            return False, f"Validation error: {str(e)}"
    
    def execute_cns(self, code: str, runner_path: Path, args=None, timeout: int = 10) -> tuple:
        """Execute CNS code"""
        temp_file = Path("/tmp/grok_test_execute.cns")
        if not code.endswith('\n'):
            code += '\n'
        temp_file.write_text(code)
        
        cmd = [str(runner_path), str(temp_file)]
        if args:
            cmd.extend(args)
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=timeout
            )
            success = result.returncode == 0
            output = result.stdout + result.stderr
            return success, output
        except Exception as e:
            return False, f"Execution error: {str(e)}"


def run_test(test_num: int, project_root: Path, tester: GrokTester):
    """Run a single test"""
    print(f"\n{'='*70}")
    print(f"TEST {test_num}")
    print(f"{'='*70}")
    
    # Paths
    prompt_file = project_root / "tests" / "grok-iterations" / "iteration-4" / f"TEST-{test_num}-FULL-PROMPT.md"
    output_dir = project_root / "tests" / "grok-iterations" / "iteration-4" / "results"
    output_dir.mkdir(exist_ok=True)
    
    validator_path = project_root / "cns-validate"
    runner_path = project_root / "cns-run"
    
    # Load prompt
    print(f"Loading prompt: {prompt_file.name}")
    prompt = prompt_file.read_text()
    print(f"Prompt length: {len(prompt)} chars ({len(prompt.split())} words)")
    
    # Generate code
    start_time = time.time()
    try:
        raw_output = tester.generate(prompt)
        generation_time = time.time() - start_time
        print(f"✓ Generated response in {generation_time:.2f}s")
    except Exception as e:
        print(f"✗ Generation failed: {e}")
        return {
            "test": test_num,
            "success": False,
            "error": str(e),
            "stage": "generation"
        }
    
    # Save raw output
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    raw_file = output_dir / f"test-{test_num}-raw-{timestamp}.txt"
    raw_file.write_text(raw_output)
    print(f"Saved raw output: {raw_file.name}")
    
    # Extract CNS code
    cns_code = tester.extract_cns_code(raw_output)
    code_file = output_dir / f"test-{test_num}-grok-{timestamp}.cns"
    if not cns_code.endswith('\n'):
        cns_code += '\n'
    code_file.write_text(cns_code)
    print(f"Extracted CNS code: {len(cns_code)} chars")
    print(f"Saved to: {code_file.name}")
    
    # Validate
    print("\nValidating...")
    valid, validation_output = tester.validate_cns(cns_code, validator_path)
    
    if valid:
        print("✓ VALIDATION PASSED")
    else:
        print("✗ VALIDATION FAILED")
        print(f"Error: {validation_output[:500]}")
        return {
            "test": test_num,
            "success": False,
            "validation_passed": False,
            "validation_output": validation_output,
            "code_file": str(code_file),
            "raw_file": str(raw_file)
        }
    
    # Execute based on test type
    print("\nExecuting...")
    execution_results = []
    
    if test_num == 1:
        # Word counter tests
        test_cases = [
            (["test-input.txt"], "default word count"),
            (["test-input.txt", "--lines"], "line count"),
            (["test-input.txt", "--chars"], "character count"),
            (["test-input.txt", "--verbose"], "verbose mode"),
            (["nonexistent.txt"], "missing file handling"),
        ]
        
        # Create test input file
        test_input = project_root / "tests" / "test-input.txt"
        if not test_input.exists():
            test_input.write_text("hello world\nthis is a test\n")
        
        for args, description in test_cases:
            print(f"  Testing: {description}")
            success, output = tester.execute_cns(cns_code, runner_path, args)
            execution_results.append({
                "description": description,
                "args": args,
                "success": success,
                "output": output
            })
            print(f"    {'✓' if success else '✗'} {output[:100]}")
    
    elif test_num == 2:
        # Job manager tests (just validate it runs)
        test_cases = [
            (["run", "echo 'test'"], "run command"),
            (["help"], "help message"),
        ]
        
        for args, description in test_cases:
            print(f"  Testing: {description}")
            success, output = tester.execute_cns(cns_code, runner_path, args, timeout=15)
            execution_results.append({
                "description": description,
                "args": args,
                "success": success,
                "output": output
            })
            print(f"    {'✓' if success else '✗'} {output[:100]}")
    
    elif test_num == 3:
        # API server test (just validate it starts)
        print("  Testing: server startup")
        success, output = tester.execute_cns(cns_code, runner_path, timeout=2)
        # For server, we expect timeout (it runs forever)
        if "timeout" in output.lower() or "Server started" in output:
            success = True
        execution_results.append({
            "description": "server startup",
            "success": success,
            "output": output
        })
        print(f"    {'✓' if success else '✗'} {output[:100]}")
    
    # Overall success
    all_executions_passed = all(r["success"] for r in execution_results)
    
    if all_executions_passed:
        print("\n✓ ALL EXECUTIONS PASSED")
    else:
        print("\n✗ SOME EXECUTIONS FAILED")
    
    # Save detailed results
    result = {
        "test": test_num,
        "timestamp": timestamp,
        "model": tester.model,
        "generation_time": generation_time,
        "validation_passed": valid,
        "execution_results": execution_results,
        "success": valid and all_executions_passed,
        "code_file": str(code_file),
        "raw_file": str(raw_file)
    }
    
    result_file = output_dir / f"test-{test_num}-result-{timestamp}.json"
    with open(result_file, 'w') as f:
        json.dump(result, f, indent=2)
    print(f"\nResults saved: {result_file.name}")
    
    return result


def main():
    # Setup
    project_root = Path(__file__).parent.parent.parent.parent
    
    # Get API key
    api_key = os.environ.get("GROK_API_KEY")
    if not api_key:
        # Try loading from .env
        env_file = project_root / ".env"
        if env_file.exists():
            for line in env_file.read_text().split('\n'):
                if line.startswith("GROK_API_KEY="):
                    api_key = line.split('=', 1)[1].strip()
                    break
    
    if not api_key:
        print("ERROR: GROK_API_KEY not found in environment or .env file")
        sys.exit(1)
    
    # Initialize tester
    tester = GrokTester(api_key)
    
    print("="*70)
    print("CNS v2.0.0 - Grok Iteration 4 Test Suite")
    print("="*70)
    print(f"Model: {tester.model}")
    print(f"Project: {project_root}")
    
    # Run all tests
    results = []
    for test_num in [1, 2, 3]:
        result = run_test(test_num, project_root, tester)
        results.append(result)
        time.sleep(2)  # Brief pause between tests
    
    # Summary
    print("\n" + "="*70)
    print("FINAL SUMMARY")
    print("="*70)
    
    for result in results:
        status = "✓ PASS" if result.get("success") else "✗ FAIL"
        print(f"Test {result['test']}: {status}")
    
    success_count = sum(1 for r in results if r.get("success"))
    print(f"\nSuccess Rate: {success_count}/3 ({success_count/3*100:.0f}%)")
    
    if success_count == 3:
        print("\n🎉 ALL TESTS PASSED!")
        sys.exit(0)
    else:
        print(f"\n⚠️  {3-success_count} test(s) failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
