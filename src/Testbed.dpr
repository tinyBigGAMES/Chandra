{==============================================================================
    ___  _                    _
   / __|| |_   __ _  _ _   __| | _ _  __ _ ™
  | (__ | ' \ / _` || ' \ / _` || '_|/ _` |
   \___||_||_|\__,_||_||_|\__,_||_|  \__,_|
           Lua Scripting for Pascal

 Copyright © 2024-present tinyBigGAMES™ LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/Chandra

 BSD 3-Clause License

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

 ------------------------------------------------------------------------------

 Testbed Program
 ======================

 This program is a demonstration of how to integrate Lua scripting with Delphi
 applications using the Chandra library. It provides practical examples of
 exposing Delphi methods and data structures to a Lua scripting environment,
 enabling dynamic behavior and extensibility in software solutions.

 Purpose:
 --------
 - To demonstrate the interoperability between Delphi and Lua.
 - To showcase how Delphi's strong typing and performance can be combined with
   Lua's flexibility and scripting capabilities.
 - To provide a hands-on example of advanced Delphi techniques, including RTTI
   (Run-Time Type Information), memory management, and script integration.

 Features:
 ---------
 - Arithmetic Operations: Includes methods for addition and multiplication,
   which are exposed to Lua scripts to showcase how Delphi logic can be invoked
   dynamically.
 - String Manipulation: Demonstrates concatenating strings and creating a list
   of strings that can be accessed or manipulated in Lua.
 - Record Handling: Illustrates creating and updating Delphi records from Lua,
   as well as retrieving their values through pointers.
 - Memory Management: Provides examples of allocating, writing to, and reading
   from memory blocks dynamically, showcasing safe pointer usage.
 - Error Handling: Integrates robust error handling to capture and display
   exceptions during script execution.
 - Lua Integration: Loads, executes, and interacts with Lua scripts that call
   Delphi functions, with full control over the execution flow.

 What You Will Learn:
 --------------------
 1. How to expose Delphi methods and records to Lua scripts using RTTI.
 2. The process of integrating Lua scripting into a Delphi application.
 3. How to handle memory safely when working with pointers in Delphi.
 4. Practical techniques for debugging and error handling in a script-
    integrated application.
 5. How to combine Delphi's static structure with Lua's dynamic flexibility for
    powerful, extensible applications.

 Lua Script Example:
 -------------------
 The program includes a Lua script (`CScript`) that demonstrates the following:
  - Arithmetic operations using `TTestClass.Add` and `TTestClass.Multiply`.
  - String concatenation using `TTestClass.Concat`.
  - Record creation and manipulation, including retrieving and updating values.
  - Memory allocation, writing, reading, and freeing within Lua scripts.

 Example Lua Usage:
 ------------------
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

 Usage Instructions:
 -------------------
 1. Compile the program in the Delphi IDE with the required Chandra library.
 2. Run the program. It will register the `TTestClass` methods with the Lua
    environment and execute the embedded Lua script (`CScript`).
 3. Observe the results of Lua interacting with Delphi methods in the console
    output.

 Key Takeaways:
 --------------
 This program is an excellent example of embedding a scripting engine within a
 Delphi application, demonstrating how to:
  - Extend application functionality through scripts.
  - Safely manage memory and complex data structures across different
    environments.
  - Debug and handle errors effectively when combining static and dynamic
    paradigms.

 Notes:
 ------
 Ensure the Lua interpreter and Chandra library are correctly set up before
 running this program. It serves as both a learning tool and a practical
 reference for Delphi developers interested in script integration.

 Created for developers looking to enhance their applications with Lua
 scripting capabilities, combining the best of Delphi's performance and Lua's
 flexibility.

 ------------------------------------------------------------------------------

Chandra Usage Notes:
====================
 1. Integrated Lua Version
    Chandra uses Lua 5.4.7+, which is statically compiled into the Delphi
    application.
    - There are no external DLL dependencies to manage, ensuring portability
       and simplicity.

 2. Automatic Registration of Delphi Routines
    Chandra automatically registers native Delphi routines that are declared as
    published class methods.
    - You don’t need to manually bind methods to Lua; simply mark the desired
      routines as `published`.

 3. Supported Parameter and Return Types
    Chandra supports only basic types and pointers for parameters and return
    values.
    - Supported types include:
        - string
        - float (single or double)
        - Boolean
        - pointer
    - When designing methods for Lua, ensure all parameters and return types
      use these supported types.

 4. Pointer Management
    Pointers created on the native (Delphi) side must be managed by native
    code.
    - For example:
        - If you create a Delphi record and pass its pointer to Lua, Lua can
          hold and reference the pointer.
        - However, only native Delphi code can modify or operate on the
          underlying data.
        - If the pointer was dynamically allocated, it must be released on the
          Delphi side.

  5. Script commands/variables:
     - Chandra.version   - Chandra version (string)
     - Chandra.luaVerion - Lua version (string)
     - dbg()             - Place in your Lua source to invokes the interactive
                           debugger

  6. Prerequisites
     - Delphi 12.2 or higher
     - Windows 10 or higher
     - Tested on Windows 11 64-bit (23H2), Delphi 12.2

==============================================================================}

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Chandra in 'Chandra.pas';

type
  /// <summary>
  /// Represents a test record with an ID, Name, and Value fields.
  /// </summary>
  /// <remarks>
  /// Used to demonstrate record creation, manipulation, and pointer-based operations in Lua scripts.
  /// </remarks>
  TTestRecord = record
    /// <summary>
    /// The unique identifier for the record.
    /// </summary>
    ID: Integer;
    /// <summary>
    /// The name associated with the record.
    /// </summary>
    Name: string;
    /// <summary>
    /// The double precision floating-point value associated with the record.
    /// </summary>
    Value: Double;
  end;

  /// <summary>
  /// Pointer type for <see cref="TTestRecord"/>.
  /// </summary>
  PTestRecord = ^TTestRecord;

  {$M+}
  /// <summary>
  /// A class containing test methods to be registered with the Lua environment.
  /// </summary>
  /// <remarks>
  /// Provides examples of arithmetic, string manipulation, record handling, and memory management for Lua scripts.
  /// </remarks>
  TTestClass = class
  published
    /// <summary>
    /// Adds two integers and returns the result.
    /// </summary>
    /// <param name="A">The first integer.</param>
    /// <param name="B">The second integer.</param>
    /// <returns>The sum of <paramref name="A"/> and <paramref name="B"/>.</returns>
    class function Add(A, B: Integer): Integer;

    /// <summary>
    /// Multiplies two double values and returns the result.
    /// </summary>
    /// <param name="A">The first double value.</param>
    /// <param name="B">The second double value.</param>
    /// <returns>The product of <paramref name="A"/> and <paramref name="B"/>.</returns>
    class function Multiply(A, B: Double): Double;

    /// <summary>
    /// Concatenates two strings and returns the combined string.
    /// </summary>
    /// <param name="A">The first string.</param>
    /// <param name="B">The second string.</param>
    /// <returns>The concatenated result of <paramref name="A"/> and <paramref name="B"/>.</returns>
    class function Concat(const A, B: string): string;

    /// <summary>
    /// Creates a new string list and populates it with sample data.
    /// </summary>
    /// <returns>A newly created <see cref="TStringList"/> instance with test data.</returns>
    class function CreateList: TStringList;

    /// <summary>
    /// Retrieves the number of items in a string list.
    /// </summary>
    /// <param name="List">The <see cref="TStringList"/> instance to count items in.</param>
    /// <returns>The number of items in the <paramref name="List"/>.</returns>
    class function GetListCount(List: TStringList): Integer;

    /// <summary>
    /// Creates a test record with the specified values.
    /// </summary>
    /// <param name="ID">The ID of the record.</param>
    /// <param name="Name">The name associated with the record.</param>
    /// <param name="Value">The value of the record.</param>
    /// <returns>A new <see cref="TTestRecord"/> with the specified values.</returns>
    class function CreateRecord(ID: Integer; const Name: string; Value: Double): TTestRecord;

    /// <summary>
    /// Updates the value field of a test record.
    /// </summary>
    /// <param name="P">Pointer to the record to update.</param>
    /// <param name="NewValue">The new value to set in the record.</param>
    class procedure UpdateRecord(P: PTestRecord; NewValue: Double);

    /// <summary>
    /// Retrieves the value field from a test record.
    /// </summary>
    /// <param name="P">Pointer to the record.</param>
    /// <returns>The value field of the record, or 0 if the pointer is nil.</returns>
    class function GetRecordValue(P: PTestRecord): Double;

    /// <summary>
    /// Allocates memory of a specified size.
    /// </summary>
    /// <param name="Size">The size of the memory block to allocate, in bytes.</param>
    /// <returns>A pointer to the allocated memory block.</returns>
    class function AllocateMemory(Size: Integer): Pointer;

    /// <summary>
    /// Frees a previously allocated memory block.
    /// </summary>
    /// <param name="P">Pointer to the memory block to free.</param>
    class procedure FreeMemory(P: Pointer);

    /// <summary>
    /// Writes an integer value to a memory block.
    /// </summary>
    /// <param name="P">Pointer to the memory block.</param>
    /// <param name="Value">The integer value to write.</param>
    class procedure WriteToMemory(P: Pointer; Value: Integer);

    /// <summary>
    /// Reads an integer value from a memory block.
    /// </summary>
    /// <param name="P">Pointer to the memory block.</param>
    /// <returns>The integer value read from the memory block, or 0 if the pointer is nil.</returns>
    class function ReadFromMemory(P: Pointer): Integer;
  end;
  {$M-}

{ TTestClass }

/// <summary>
/// Adds two integers and returns the result.
/// </summary>
/// <param name="A">The first integer.</param>
/// <param name="B">The second integer.</param>
/// <returns>The sum of <paramref name="A"/> and <paramref name="B"/>.</returns>
class function TTestClass.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

/// <summary>
/// Multiplies two double values and returns the result.
/// </summary>
/// <param name="A">The first double value.</param>
/// <param name="B">The second double value.</param>
/// <returns>The product of <paramref name="A"/> and <paramref name="B"/>.</returns>
class function TTestClass.Multiply(A, B: Double): Double;
begin
  Result := A * B;
end;

/// <summary>
/// Concatenates two strings and returns the combined result.
/// </summary>
/// <param name="A">The first string.</param>
/// <param name="B">The second string.</param>
/// <returns>The concatenated result of <paramref name="A"/> and <paramref name="B"/>.</returns>
class function TTestClass.Concat(const A, B: string): string;
begin
  Result := A + B;
end;

/// <summary>
/// Creates a new <see cref="TStringList"/> and populates it with sample data.
/// </summary>
/// <returns>A newly created <see cref="TStringList"/> containing test data.</returns>
class function TTestClass.CreateList: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Test1');
  Result.Add('Test2');
end;

/// <summary>
/// Retrieves the number of items in a given string list.
/// </summary>
/// <param name="List">The <see cref="TStringList"/> whose item count is to be retrieved.</param>
/// <returns>The number of items in the <paramref name="List"/>.</returns>
class function TTestClass.GetListCount(List: TStringList): Integer;
begin
  Result := List.Count;
end;

/// <summary>
/// Creates a <see cref="TTestRecord"/> with the specified values.
/// </summary>
/// <param name="ID">The ID of the record.</param>
/// <param name="Name">The name associated with the record.</param>
/// <param name="Value">The double value to be assigned to the record.</param>
/// <returns>A <see cref="TTestRecord"/> with the specified values.</returns>
class function TTestClass.CreateRecord(ID: Integer; const Name: string; Value: Double): TTestRecord;
begin
  Result.ID := ID;
  Result.Name := Name;
  Result.Value := Value;
end;

/// <summary>
/// Updates the value field of a <see cref="TTestRecord"/> through its pointer.
/// </summary>
/// <param name="P">Pointer to the record to update.</param>
/// <param name="NewValue">The new value to assign to the record.</param>
class procedure TTestClass.UpdateRecord(P: PTestRecord; NewValue: Double);
begin
  if P <> nil then
    P^.Value := NewValue;
end;

/// <summary>
/// Retrieves the value field from a <see cref="TTestRecord"/> using its pointer.
/// </summary>
/// <param name="P">Pointer to the record.</param>
/// <returns>The value field of the record, or 0 if the pointer is nil.</returns>
class function TTestClass.GetRecordValue(P: PTestRecord): Double;
begin
  if P <> nil then
    Result := P^.Value
  else
    Result := 0;
end;

/// <summary>
/// Allocates a memory block of the specified size.
/// </summary>
/// <param name="Size">The size of the memory block to allocate, in bytes.</param>
/// <returns>A pointer to the allocated memory block.</returns>
class function TTestClass.AllocateMemory(Size: Integer): Pointer;
begin
  Result := AllocMem(Size);
end;

/// <summary>
/// Frees a previously allocated memory block.
/// </summary>
/// <param name="P">Pointer to the memory block to free.</param>
class procedure TTestClass.FreeMemory(P: Pointer);
begin
  FreeMem(P);
end;

/// <summary>
/// Writes an integer value to a specified memory block.
/// </summary>
/// <param name="P">Pointer to the memory block.</param>
/// <param name="Value">The integer value to write to the memory block.</param>
class procedure TTestClass.WriteToMemory(P: Pointer; Value: Integer);
begin
  if P <> nil then
    PInteger(P)^ := Value;
end;

/// <summary>
/// Reads an integer value from a specified memory block.
/// </summary>
/// <param name="P">Pointer to the memory block.</param>
/// <returns>The integer value read from the memory block, or 0 if the pointer is nil.</returns>
class function TTestClass.ReadFromMemory(P: Pointer): Integer;
begin
  if P <> nil then
    Result := PInteger(P)^
  else
    Result := 0;
end;

const
  /// <summary>
  /// A Lua script that tests the functionality of the <see cref="TTestClass"/> methods,
  /// and general Chandra functionality
  /// </summary>
  CScript =
  '''
  --dbg() -- uncomment to debug script
  local mm = import("./res/scripts/mymath.lua")
  mm.add(50,50)
  test_var = 0
  test_table = {test="test"}
  print("test_var: " .. test_var)
  print("test_table: " .. test_table.test)
  print("Lua Version: " .. Chandra.luaVersion)
  print("Chandra version: " .. Chandra.version)

  print("Testing TTestClass methods...")
  print("Add:", TTestClass.Add(5, 3))
  print("Multiply:", TTestClass.Multiply(4.5, 2.0))
  print("Concat:", TTestClass.Concat("Hello ", "World"))
  local list = TTestClass.CreateList()
  print("List count:", TTestClass.GetListCount(list))

  -- Record pointer example
  local rec = TTestClass.CreateRecord(6889, "Test", 3.14)
  print("Initial Value:", TTestClass.GetRecordValue(rec))
  TTestClass.UpdateRecord(rec, 6.28)
  print("Updated Value:", TTestClass.GetRecordValue(rec))

  -- Regular pointer example
  local ptr = TTestClass.AllocateMemory(4)
  TTestClass.WriteToMemory(ptr, 42)
  print("Memory Value:", TTestClass.ReadFromMemory(ptr))
  TTestClass.FreeMemory(ptr)

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
  ''';

var
  /// <summary>
  /// An instance of the TChandra class, used to interface with the Lua scripting engine.
  /// </summary>
  /// <remarks>
  /// LChandra manages the Lua state, registers Delphi routines, and executes Lua scripts.
  /// It is the primary component enabling communication between Delphi and Lua in this program.
  /// </remarks>
  LChandra: TChandra;

  /// <summary>
  /// A variable of type TTestRecord, used for testing record handling functionality.
  /// </summary>
  /// <remarks>
  /// LRec is a Delphi record that demonstrates creating, updating, and retrieving values
  /// using both native Delphi code and Lua scripting. It serves as an example of how
  /// Delphi records can be manipulated within Lua scripts.
  /// </remarks>
  LRec: TTestRecord;

  /// <summary>
  /// Memory stream for storing compiled Lua bytecode
  /// </summary>
  LStream: TMemoryStream;

begin
  /// <summary>
  /// Entry point for the Testbed program.
  /// </summary>
  LChandra := TChandra.Create();
  try
    try
      /// <summary>
      /// Executes the stored Lua payload and exits the current procedure if successful.
      /// </summary>
      /// <remarks>
      /// This line attempts to execute the stored Lua payload using the `RunPayload` method of the
      /// `TChandra` object. If the payload execution succeeds (i.e., `RunPayload` returns <c>True</c>),
      /// the program will exit the current procedure or function early using the `Exit` statement.
      /// </remarks>
      /// <example>
      /// <code>
      /// if LChandra.RunPayload() then
      ///   Exit;
      /// </code>
      /// </example>
      /// <para>
      /// Usage Context:
      /// </para>
      /// - Ensure that the payload (Lua bytecode) has been successfully compiled and stored using
      ///   the `StorePayload` method before calling `RunPayload`.
      /// - Consider checking for payload existence using `PayloadExist` before attempting to run it.
      /// </remarks>
      /// <exception>
      /// If no payload exists or if an error occurs during execution, `RunPayload` will return <c>False</c>,
      /// and the procedure will continue executing subsequent lines.
      /// </exception>
      if LChandra.RunPayload() then
        Exit;

      /// <summary>
      /// Add search paths for Lua modules
      /// </summary>
      LChandra.AddSearchPath('.\res\scripts');

      /// <summary>
      /// Registers the routines of <see cref="TTestClass"/> with the Lua state and loads the script.
      /// </summary>
      LChandra.RegisterRoutines(TTestClass);
      LChandra.LoadString(CScript);

      LChandra.PrintLn('Integer value: %d', [LChandra.Call('add', [50, 50]).AsInteger]);
      LChandra.PrintLn('String value: %s', [LChandra.Call('concat', ['Hello, ', 'World!']).AsString]);

      /// <summary>
      /// Demonstrates calling Lua functions with a record pointer and additional parameters.
      /// </summary>
      LRec.ID := 1;
      LRec.Name := 'test';
      LRec.Value := 200;
      LChandra.Call('process_record', [@LRec, 'test string']);

      /// <summary>
      /// Demonstrates calling a Lua function defined as part of a table or object-like structure, passing parameters,
      /// and retrieving the result as a Delphi floating-point value. This showcases how Lua functions can be dynamically
      /// invoked using the `Call` method with support for parameters and proper type conversion of the result.
      /// </summary>
      LChandra.PrintLn('TTestClass.Multiply(2,2): %3.2f', [LChandra.Call('TTestClass.Multiply', [2, 2]).AsExtended]);

      /// <summary>
      /// Demonstrates calling a standard Lua library function, passing a parameter, and retrieving the result as a Delphi
      /// floating-point value. This highlights the flexibility of the `Call` method in invoking any valid Lua function,
      /// including built-in mathematical operations from Lua's standard library.
      /// </summary>
      LChandra.PrintLn('math.sqrt(25): %3.2f', [LChandra.Call('math.sqrt', [25]).AsExtended]);

      /// <summary>
      /// Demonstrates checking the existence of a Lua function within a table using `RoutineExist`.
      /// This example validates whether `TTestClass.Multiply` is a callable Lua routine.
      /// </summary>
      LChandra.PrintLn('Routine "TTestClass.Multiply" exist: %s', [BoolToStr(LChandra.RoutineExist('TTestClass.Multiply'), True)]);

      /// <summary>
      /// Demonstrates checking the existence of a global Lua function using `RoutineExist`.
      /// This example validates whether the global Lua function `add` exists.
      /// </summary>
      LChandra.PrintLn('Routine "add" exist: %s', [BoolToStr(LChandra.RoutineExist('add'), True)]);

      /// <summary>
      /// Demonstrates setting a value in Lua using `SetVariable`.
      /// This example assigns the string value `'test'` to the variable `v.test`,
      /// dynamically creating any missing tables.
      /// </summary>
      LChandra.PrintLn('Set variable: v.text to "test"', []);
      LChandra.SetVariable('v.test', 'test');

      /// <summary>
      /// Demonstrates retrieving a value from Lua using `GetVariable`.
      /// This example retrieves the value of `v.test` from the Lua state and outputs it as a string.
      /// </summary>
      LChandra.PrintLn('Table variable v.test: "%s"', [LChandra.GetVariable('v.test').AsString]);

      /// <summary>
      /// Demonstrates setting a Lua global variable using `SetVariable` and retrieving its value using `GetVariable`.
      /// This example assigns the value 502 to the global variable `test_var` and retrieves it as a Delphi extended type.
      /// </summary>
      LChandra.SetVariable('test_var', 502);
      LChandra.PrintLn('test_var: "%3.2f"', [LChandra.GetVariable('test_var').AsExtended]);

      /// <summary>
      /// Demonstrates retrieving a nested Lua table field using `GetVariable`.
      /// This example retrieves the value of the field `test` within the table `test_table` as a Delphi string.
      /// Ensure that the table `test_table` and its field `test` exist in the Lua state before calling `GetVariable`.
      /// </summary>
      LChandra.PrintLn('test_table.test: "%s"', [LChandra.GetVariable('test_table.test').AsString]);

      /// <summary>
      /// Resets the LChandra environment, clearing all previously loaded scripts and variables.
      /// </summary>
      LChandra.Reset();

      /// <summary>
      /// Add search paths for Lua modules
      /// </summary>
      LChandra.AddSearchPath('.\res\scripts');

      /// <summary>
      /// Copy Testbed.exe to Payload.exe, compile script file to bytecode and
      /// store in Payload.exe. Now when you run Poyload.exe it will check for
      /// a payload and execute it.
      /// </summary>
      TFile.Copy('Testbed.exe', 'Payload.exe', True);
      if LChandra.StorePayload('.\res\scripts\compiled.lua', 'Payload.exe') then
        LChandra.PrintLn('Saved bytecode to "Payload.exe"', [])
      else
        LChandra.PrintLn('Failed to save bytecode to "Payload.exe"', []);

      /// <summary>
      /// Resets the LChandra environment, clearing all previously loaded scripts and variables.
      /// </summary>
      LChandra.Reset();

      /// <summary>
      /// Creates a memory stream to hold the compiled Lua bytecode.
      /// </summary>
      LStream := TMemoryStream.Create();
      try
        /// <summary>
        /// Compiles a Lua script to a binary bytecode format and writes it to a memory stream.
        /// </summary>
        LChandra.CompileToStream('.\res\scripts\compiled.lua', LStream, False);

        /// <summary>
        /// Loads compiled Lua bytecode from a memory stream into LChandra for execution.
        /// </summary>
        LChandra.LoadBuffer(LStream.Memory, LStream.Size);
      finally
        /// <summary>
        /// Frees the memory stream to release resources.
        /// </summary>
        LStream.Free();
      end;

      /// <summary>
      /// Resets the LChandra environment, clearing all previously loaded scripts and variables.
      /// </summary>
      LChandra.Reset();

      /// <summary>
      /// Load a script from the filesystem and automatically execute it
      /// </summary>
      LChandra.LoadFile('.\res\scripts\compiled.lua');

    except
      on E: Exception do
      begin
        /// <summary>
        /// Print any error messages.
        /// </summary>
        LChandra.PrintLn('Error: %s', [E.Message]);
      end;
    end;
  finally
    /// <summary>
    /// Releases resources used by <see cref="TChandra"/>.
    /// </summary>
    LChandra.Free();
  end;

  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  WriteLn;
end.
