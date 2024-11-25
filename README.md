![Chandra](media/chandra.png)  
[![Chat on Discord](https://img.shields.io/discord/754884471324672040?style=for-the-badge)](https://discord.gg/tPWjMwK)
[![Follow on Bluesky](https://img.shields.io/badge/Bluesky-tinyBigGAMES-blue?style=for-the-badge&logo=bluesky)](https://bsky.app/profile/tinybiggames.com)

# Chandra - Lua Scripting for Delphi 🛠️

Chandra is a Lua 📜 integration library tailored specifically for Delphi 🛠️ developers. It enables seamless interaction 🔄 between Delphi applications and Lua scripts, combining Delphi's robustness 💪 with Lua's flexibility 🤸. Chandra is designed for professional developers who need to add dynamic ⚡ capabilities to their software without sacrificing performance 🚀 and stability 🔒. This comprehensive guide 📖 aims to help you make the most out of Chandra in your projects.

## Overview 📝

Chandra integrates Lua 5.4.7+, statically compiled into your Delphi application, with no external DLL dependencies 📦 to manage. This enhances portability 🚚 and simplifies distribution, making it easier to integrate scripting capabilities into your Delphi applications.

With Chandra, you can easily expose Delphi classes, methods, and data structures to Lua scripts, giving your applications dynamic behavior 🌀 and extensibility. The library leverages Delphi's RTTI (Run-Time Type Information) 🧠 to facilitate method registration, providing a streamlined process for integrating Lua.

## Key Features ✨

- **Integrated Lua Version**: Lua 5.4.7+ is embedded directly, eliminating external dependencies 📦.
- **Automatic Routine Registration** 🔄: Published Delphi methods are automatically available to Lua, simplifying the process of scripting your application.
- **Basic Types Support** 🧩: Supports parameters and return types such as strings 📝, floating-point numbers 🔢, Booleans ✅❌, and pointers ➡️.
- **Pointer Handling** 🔧: Safe pointer management ensures stability 🔒 when working with complex data structures.
- **Error Handling** ⚠️: Robust error handling during script execution is included, ensuring your application runs smoothly 🚀 even when Lua scripts fail.
- **Interactive Debugging** 🐞: Add `dbg()` in Lua code to begin debugging, useful for identifying issues during runtime.
- **Script Importing and Bundling** 📦: Use a custom `import` command to combine scripts, compile them into a single file, and optionally store them as an EXE resource for a fully self-contained application.

## Usage Instructions 🛠️

To get started with Chandra, follow these steps:

1. **Compile** 📦 your program in the Delphi IDE with the Chandra library included.
2. **Run** ▶️ the application to register the Delphi classes and methods with the Lua runtime.
3. **Write and Load Lua Scripts** 📝📜 that interact with your Delphi classes using the integrated scripting capabilities.

## Example Integration: Testbed Program 🧪

The Testbed Program is a sample project demonstrating how to use Chandra to integrate Lua scripting with a Delphi application. It provides practical examples, including arithmetic operations ➕, string manipulation ✂️, record handling 🗂️, memory management 💾, and error handling ⚠️.

### Features Demonstrated 🧩

- **Arithmetic Operations** ➕: Methods for addition and multiplication are exposed to Lua scripts.
- **String Manipulation** ✂️: Concatenates strings and creates lists accessible in Lua.
- **Record Handling** 🗂️: Create and update Delphi records from Lua, and retrieve their values via pointers.
- **Memory Management** 💾: Allocate, write, read, and free memory blocks from Lua scripts, demonstrating pointer management.
- **Lua Integration** 📜: Load, execute, and interact with Lua scripts calling Delphi functions.

### Example Lua Script 📜

```lua
print("Testing TTestClass methods...")
print("Add:", TTestClass.Add(5, 3))
print("Multiply:", TTestClass.Multiply(4.5, 2.0))
print("Concat:", TTestClass.Concat("Hello ", "World"))

-- Record handling example
local rec = TTestClass.CreateRecord(1, "Test Record", 42.0)
print("Initial Record Value:", TTestClass.GetRecordValue(rec))
TTestClass.UpdateRecord(rec, 100.0)
print("Updated Record Value:", TTestClass.GetRecordValue(rec))

-- Memory management example
local mem = TTestClass.AllocateMemory(4)
TTestClass.WriteToMemory(mem, 12345)
print("Memory Value:", TTestClass.ReadFromMemory(mem))
TTestClass.FreeMemory(mem)  

function add(a, b)
  return a + b
end

function concat(str1, str2)
  return str1 .. str2
end

function process_record(rec, str)
  print("Value:", TTestClass.GetRecordValue(rec))
  print("str: " .. str)
end
```

### Delphi Side Example 🛠️

Below is an example of how to use Chandra in a Delphi application:

```delphi  
  
type
  TTestRecord = record
    ID: Integer;
    Name: string;
    Value: Double;
  end;
  PTestRecord = ^TTestRecord;

  {$M+}
  TTestClass = class
  published
    class function Add(A, B: Integer): Integer;
    class function Multiply(A, B: Double): Double;
    class function Concat(const A, B: string): string;
    class function CreateList: TStringList;
    class function GetListCount(List: TStringList): Integer;
    class function CreateRecord(ID: Integer; const Name: string; Value: Double): TTestRecord;
    class procedure UpdateRecord(P: PTestRecord; NewValue: Double);
    class function GetRecordValue(P: PTestRecord): Double;
    class function AllocateMemory(Size: Integer): Pointer;
    class procedure FreeMemory(P: Pointer);
    class procedure WriteToMemory(P: Pointer; Value: Integer);
    class function ReadFromMemory(P: Pointer): Integer;
  end;
  {$M-}  

var
  LChandra: TChandra;
  LRec: TTestRecord;

begin
  LChandra := TChandra.Create();
  try
    try
      LChandra.RegisterRoutines(TTestClass);
      LChandra.LoadString(CScript);

      WriteLn('Integer value: ', LChandra.Call('add', [50, 50]).AsInteger);
      WriteLn('String value: ', LChandra.Call('concat', ['Hello, ', 'World!']).AsString);

      LRec.ID := 1;
      LRec.Name := 'test';
      LRec.Value := 200;

      LChandra.Call('process_record', [@LRec, 'test']);
    except
      on E: Exception do
      begin
        WriteLn(Format('Error: %s', [E.Message]));
      end;
    end;
  finally
    LChandra.Free();
  end;
end;
```

### Key Takeaways 📌

This example showcases the powerful interoperability 🔗 between Delphi and Lua. It highlights how you can:

- **Extend Application Functionality** ⚡: Use Lua scripts to add dynamic features to your Delphi applications.
- **Safe Memory Management** 💾: Demonstrates best practices for handling pointers and memory between Delphi and Lua.
- **Error Handling and Debugging** 🐞: Learn practical techniques to debug and handle errors when integrating scripting into Delphi applications.

## Advanced Usage Notes 🧠

### Integrated Lua Version 🐍

Chandra uses Lua 5.4.7+, which is statically compiled into the Delphi application. This approach eliminates the need for external DLLs 📦, ensuring your applications remain self-contained.

### Automatic Registration of Delphi Routines 🔄

Chandra automatically registers Delphi routines declared as published class methods. You don’t need to manually bind methods to Lua—simply declare them as `published`, and Chandra makes them available for scripting.

### Supported Parameter and Return Types ✅

Chandra supports basic types for method parameters and return values:

- `string` 📝
- `float` (single or double) 🔢
- `Boolean` ✅❌
- `Pointer` ➡️

When designing methods for use with Lua, ensure parameters and return values are restricted to these types.

### Pointer Management 🔧

Pointers created on the Delphi side must be managed by Delphi code:

- Pointers passed to Lua can be referenced within Lua scripts, but modification or memory operations must be handled by Delphi.
- Dynamically allocated pointers need proper cleanup to avoid memory leaks 💧.

### Prerequisites 📋

- **Delphi Version**: Delphi 12.2 or higher is required.
- **Operating System**: Windows 10 or higher; tested on Windows 11 64-bit (23H2).

## Getting Started with the Chandra Testbed Program 🚀

The Testbed Program is an example application included with Chandra. It demonstrates how to:

- **Integrate Lua scripting** 📜 into a Delphi application.
- **Expose Delphi methods** 🛠️ and data structures to Lua.
- **Combine static and dynamic programming paradigms** 🔄 for powerful application extensibility.

### How to Run the Testbed Program ▶️

1. Compile the program in the Delphi IDE, ensuring that the Chandra library is correctly included.
2. Run the program to see how Lua interacts with Delphi methods.
3. Observe console outputs that demonstrate arithmetic operations ➕, record handling 🗂️, and memory management 💾.

### Example Workflow 🔄

- **Arithmetic with Lua** ➕: Lua scripts call Delphi methods to perform addition and multiplication.
- **String Operations** ✂️: Lua can concatenate strings using methods exposed from Delphi.
- **Record Management** 🗂️: Delphi records can be created and updated via Lua scripts, showing how to pass complex data between the two environments.
- **Memory Management** 💾: Demonstrates safe handling of dynamically allocated memory through pointers.

## Notes and Recommendations 📝

- **Lua Interpreter Setup** 🐍: Ensure the embedded Lua version is correctly linked and compiled with the Delphi project.
- **Pointer Safety** 🔒: Pointers passed to Lua should be managed exclusively by Delphi to prevent memory issues.
- **Documentation** 📖: Thoroughly document any methods exposed to Lua, specifying the intended usage and parameter types.

## Conclusion 🎯

Chandra offers a straightforward solution for integrating Lua scripting 📜 into Delphi applications, providing developers with a way to leverage the flexibility 🤸 of Lua without sacrificing the strong typing and performance 🚀 advantages of Delphi. By using Chandra, you can significantly enhance your applications' extensibility, maintainability 🛠️, and customization potential.

Whether you're building complex applications that require runtime customization or just want to offer script-based configuration options, Chandra makes Lua scripting integration both accessible and powerful 💪 for Delphi developers.

## Requirements ✅

- **Delphi 12.2 or higher** 🛠️
- **Windows 10 or higher** 🖥️ (Tested on Windows 11 64-bit, version 23H2)

### Contributing

Contributions to **Chandra** are highly encouraged. Please feel free to submit issues, suggest new features, or create pull requests to expand the capabilities and robustness of the scripting engine.

### License

**Chandra** is distributed under the 🆓 **BSD-3-Clause License**, allowing for redistribution and use in both source and binary forms, with or without modification, under specific conditions. See the [LICENSE](https://github.com/tinyBigGAMES/Chandra?tab=BSD-3-Clause-1-ov-file#BSD-3-Clause-1-ov-file) file for more details.

### Support

- <a href="https://github.com/tinyBigGAMES/Chandra/issues" target="_blank">Issues</a>
- <a href="https://github.com/tinyBigGAMES/Chandra/discussions" target="_blank">Discussions</a>
- <a href="https://learndelphi.org/" target="_blank">Learn Delphi</a>
- <a href="https://www.lua.org/start.html" target="_blank">Learn Lua</a>
---
For any professional Delphi developer interested in enhancing application flexibility with scripting capabilities, Chandra offers a tested and reliable solution that keeps everything self-contained and performant. 🚀

<p align="center">
<img src="media/delphi.png" alt="Delphi">
</p>
<h5 align="center">

Made with :heart: in Delphi
</h5>

