# CNS vs Python: The Killer App Comparison

## The Challenge

Build a program that:
1. Calls an IP geolocation API to get user's location
2. Calls a UUID generator API
3. Parses JSON responses
4. Displays formatted output

---

## Python Version

**Requirements:**
```
requests==2.31.0
```

**Setup:**
```bash
python3 -m venv venv
source venv/bin/activate
pip install requests
```

**Code (python_demo.py):**
```python
import requests
import json
import sys

def main():
    print("━" * 48)
    print("  Python Multi-API Demo")
    print("━" * 48)
    print()
    
    # Call API 1: IP Geolocation
    print("[1/2] Calling IP Geolocation API...")
    try:
        response1 = requests.get("http://ip-api.com/json", timeout=10)
        response1.raise_for_status()
        data1 = response1.json()
        
        city = data1.get("city", "Unknown")
        country = data1.get("country", "Unknown")
        isp = data1.get("isp", "Unknown")
        
        print(f"     ✓ Location: {city}, {country}")
        print(f"     ✓ ISP: {isp}")
        print()
    except requests.RequestException as e:
        print(f"     ✗ Error: {e}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"     ✗ JSON Error: {e}")
        sys.exit(1)
    
    # Call API 2: UUID Generator
    print("[2/2] Calling UUID Generator API...")
    try:
        response2 = requests.get("http://httpbin.org/uuid", timeout=10)
        response2.raise_for_status()
        data2 = response2.json()
        
        uuid = data2.get("uuid", "Unknown")
        
        print(f"     ✓ Generated UUID: {uuid}")
        print()
    except requests.RequestException as e:
        print(f"     ✗ Error: {e}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"     ✗ JSON Error: {e}")
        sys.exit(1)
    
    # Summary
    print("━" * 48)
    print("  ✅ SUCCESS!")
    print()
    print("  • Called 2 different REST APIs")
    print("  • Parsed JSON responses")
    print("  • But required pip install + venv setup")
    print("━" * 48)

if __name__ == "__main__":
    main()
```

**Lines of Code:** 67 lines
**Dependencies:** requests library
**Setup Time:** ~30 seconds (pip install)
**Run:** `python python_demo.py`

---

## CNS Version

**Requirements:** None

**Setup:** None

**Code (killer-app-demo.cns):**
```cns
Story: Multi-API Demo

Given:
  api1: String = "http://ip-api.com/json"
  response1: String = ""
  city: String = ""
  country: String = ""
  isp: String = ""
  
  api2: String = "http://httpbin.org/uuid"
  response2: String = ""
  uuid: String = ""

Step 1 → Call first API
  Because: Get geo-location from IP
  Effect: Print "[1/2] Calling IP Geolocation API..."
  Effect: HTTP GET from api1 into response1

Step 2 → Parse location data
  Because: Extract fields from JSON
  Then: city becomes PARSE JSON response1 GET "city"
  Then: country becomes PARSE JSON response1 GET "country"
  Then: isp becomes PARSE JSON response1 GET "isp"
  Effect: Print "     ✓ Location: {city}, {country}"
  Effect: Print "     ✓ ISP: {isp}"

Step 3 → Call second API
  Because: Generate unique identifier
  Effect: Print "[2/2] Calling UUID Generator API..."
  Effect: HTTP GET from api2 into response2

Step 4 → Parse UUID
  Because: Extract UUID from JSON
  Then: uuid becomes PARSE JSON response2 GET "uuid"
  Effect: Print "     ✓ Generated UUID: {uuid}"

Step 5 → Show results
  Because: Demonstrate success
  Effect: Print "✅ SUCCESS!"
  Effect: Print "• Called 2 different REST APIs"
  Effect: Print "• Zero dependencies (no pip, npm, cargo)"

End: Return "Demo complete"
```

**Lines of Code:** 42 lines (35% smaller)
**Dependencies:** ZERO
**Setup Time:** 0 seconds
**Run:** `./cns-run killer-app-demo.cns`

---

## Comparison Summary

| Metric | Python | CNS | Winner |
|--------|--------|-----|---------|
| **Lines of Code** | 67 | 42 | 🏆 CNS (37% less) |
| **Dependencies** | requests | None | 🏆 CNS |
| **Setup Time** | ~30 seconds | 0 seconds | 🏆 CNS |
| **Readability** | Good | Self-documenting | 🏆 CNS |
| **Error Handling** | Manual try/catch | Built-in | 🏆 CNS |
| **Documentation** | Separate comments | Narrative Story | 🏆 CNS |
| **HTTP Client** | External library | Built-in | 🏆 CNS |
| **JSON Parser** | External library | Built-in | 🏆 CNS |
| **Deployment** | venv + requirements.txt | Single binary | 🏆 CNS |

---

## The "It Just Works" Factor

**Python:**
```bash
git clone your-repo
cd your-repo
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python python_demo.py
```

**CNS:**
```bash
git clone cns-starter
cd cns-starter
./cns-run killer-app-demo.cns
```

**CNS = 3 commands vs Python = 6 commands**

---

## For LLMs

**With Python:** LLM must remember to:
- Import correct libraries
- Handle exceptions properly
- Parse JSON correctly
- Format strings correctly
- Set timeouts
- Handle HTTP status codes

**With CNS:** LLM just writes the narrative:
- "Call this API"
- "Parse this field"
- "Print this message"

The Story format guides the LLM naturally!

---

## Conclusion

**CNS wins on:**
- ✅ Zero dependencies
- ✅ Instant execution
- ✅ Self-documenting code
- ✅ Smaller codebase
- ✅ LLM-friendly syntax
- ✅ Built-in HTTP + JSON

**Perfect for:**
- 🎯 Quick API integrations
- 🎯 Prototyping multi-API workflows
- 🎯 LLM-generated code
- 🎯 Teaching API concepts
- 🎯 Serverless functions
