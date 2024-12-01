{==============================================================================
    ___  _                    _
   / __|| |_   __ _  _ _   __| | _ _  __ _ ™
  | (__ | ' \ / _` || ' \ / _` || '_|/ _` |
   \___||_||_|\__,_||_||_|\__,_||_|  \__,_|
           Lua Scripting for Delphi

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
 This project used the following open-source libraries:
  - Lua (https://github.com/lua/lua)

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

 5. Script Commands/Variables:
    - Chandra.version      - Chandra version (string)
    - Chandra.luaVersion  - Lua version (string)
    - dbg()               - Place in your Lua source to invoke the interactive
                            debugger

 6. Prerequisites
    - Delphi 12.2 or higher
    - Windows 10 or higher
    - Tested on Windows 11 64-bit (23H2), Delphi 12.2

 7. Lua Garbage Collection (GC) Management
    Effective memory management is crucial for maintaining optimal performance
    in applications that embed Lua. Chandra provides a set of routines to
    control and monitor Lua's garbage collector (GC) directly from Delphi.
    Below are detailed explanations of these routines, including when and how
    to use them.

    7.1. SetGCStepSize(const AStep: Integer)
         What It Does:
           Sets the step multiplier for Lua's garbage collector. The step
           multiplier determines the amount of work the GC performs in each
           incremental step, influencing its aggressiveness.

         When to Use It:
           - Performance Optimization: Increase the step size to make GC
             more aggressive if memory usage is high.
           - Reducing Latency: Decrease the step size to spread GC workload,
             minimizing pauses in performance-critical applications.
           - Memory-Constrained Environments: Adjust step size to better manage
             limited memory resources.

         How to Use It:
           // Example: Setting the GC step size to 200%
           SetGCStepSize(200);

         Parameters:
           - AStep: A positive integer representing the GC step multiplier.
                    Lua's default is typically around 200. Higher values make
                    GC more aggressive.

         Considerations:
           - Balance: Too high a value may increase CPU usage, while too low
             may lead to inadequate garbage collection.
           - Testing: Experiment with different values to find the optimal
             balance for your application.

    7.2. GetGCStepSize(): Integer
         What It Does:
           Retrieves the current step multiplier value of Lua's garbage
           collector, allowing you to monitor the GC's configuration.

         When to Use It:
           - Monitoring: Keep track of the current GC settings.
           - Debugging: Diagnose memory-related issues by understanding GC
             behavior.
           - Dynamic Adjustments: Inform further adjustments based on runtime
             conditions.

         How to Use It:
           var
             CurrentStepSize: Integer;
           begin
             CurrentStepSize := GetGCStepSize();
             ShowMessage('Current GC Step Size: ' + IntToStr(CurrentStepSize));
           end;

         Returns:
           - An integer representing the current GC step multiplier.

         Considerations:
           - Regularly check to ensure GC is configured as intended, especially
             in complex applications.

    7.3. GetGCMemoryUsed(): Integer
         What It Does:
           Returns the amount of memory currently used by Lua's garbage
           collector, measured in bytes.

         When to Use It:
           - Memory Monitoring: Track memory usage trends to identify leaks or
             excessive consumption.
           - Performance Tuning: Use memory usage data to adjust GC settings.
           - Resource Management: Ensure memory usage stays within acceptable
             limits in constrained environments.

         How to Use It:
           var
             MemoryUsed: Integer;
           begin
             MemoryUsed := GetGCMemoryUsed();
             ShowMessage('Lua GC Memory Used: ' + IntToStr(MemoryUsed) +
              ' bytes');
           end;

         Returns:
           - An integer representing the memory usage of Lua's GC in bytes.

         Considerations:
           - Combine memory data with GC step size and performance metrics for
             informed memory management decisions.

    7.4. CollectGarbage()
         What It Does:
           Initiates an immediate garbage collection cycle in Lua, forcing the
           GC to reclaim memory from unused objects.

         When to Use It:
           - Explicit Memory Management: Trigger GC during moments when
             temporary pauses are acceptable, such as after loading large
             datasets.
           - Resource Cleanup: Free up memory promptly after operations that
             generate significant temporary objects.
           - Manual Control: Supplement automated GC triggers to maintain
             optimal performance.

         How to Use It:
           begin
             CollectGarbage();
             ShowMessage('Lua garbage collection cycle initiated.');
           end;

         Considerations:
           - Performance Impact: Forcing GC can cause temporary pauses; use
             judiciously to avoid negatively impacting user experience.
           - Timing: Identify suitable application moments, like idle times, to
             perform manual GC.
           - Complementary Use: Combine manual GC with automated settings for
             balanced memory management.

    Detailed Guidance on Using Lua GC Management Routines

    Overview of Lua's Garbage Collector
      Lua's incremental garbage collector automatically manages memory by
      reclaiming unused objects in small steps to prevent long pauses.
      Adjusting the GC's behavior can optimize memory usage and application
      responsiveness.

    Best Practices and Considerations
      1. Understand Lua's GC Mechanics:
         - Familiarize yourself with Lua's incremental garbage collection to
            make informed adjustments.

      2. Avoid Overusing Manual GC Triggers:
         - Excessive CollectGarbage calls can degrade performance. Use them
           sparingly.

      3. Monitor Application Performance:
         - Assess the impact of GC adjustments on both memory usage and
           responsiveness.

      4. Test Across Scenarios:
         - Different workloads may respond differently to GC settings. Conduct
           thorough testing.

      5. Handle GC States Appropriately:
         - Ensure your application manages state changes introduced by garbage
           collection, especially with weak references.

      6. Stay Updated with Lua Versions:
         - GC behavior may vary between Lua versions. Ensure compatibility with
           the Lua version used by Chandra.

    Example Usage in a Delphi Application
      Below is a practical example demonstrating how to integrate and utilize
      the GC management routines within a Delphi application interfacing with
      Lua via Chandra.

       uses
        Chandra;

       var
         LuaState: PLua_State; // Assume this is initialized elsewhere

     Usage Example:
       procedure TForm1.ButtonOptimizeGCClick(Sender: TObject);
       begin
         try
           // Set GC step size to 150%
           SetGCStepSize(150);
           ShowMessage('GC Step Size set to 150%.');

           // Retrieve and display current step size
           ShowMessage('Current GC Step Size: ' + IntToStr(GetGCStepSize()));

           // Check memory usage
           ShowMessage('Lua GC Memory Used: ' + IntToStr(GetGCMemoryUsed()) +
             ' bytes');

           // Force a garbage collection cycle
           CollectGarbage();
           ShowMessage('Garbage collection cycle initiated.');
         except
           on E: Exception do
             ShowMessage('Error: ' + E.Message);
         end;
       end;

  Additional Notes
    - Lua Integration: Ensure that the Lua state (LuaState) is correctly
      initialized and managed within your application.

    - Error Handling: Implement robust error handling to manage scenarios where
      GC operations might fail or behave unexpectedly.

    - Performance Considerations: Adjusting the GC's step size can
      significantly impact application performance and memory usage. Test
      different configurations to identify the optimal settings for your use
      case.

 ------------------------------------------------------------------------------

>>> CHANGELOG <<<

Version 0.1.0
-------------
  - Initial release.

==============================================================================}

unit Chandra;

{$IF CompilerVersion >= 36.0}
  // Code specific to Delphi Athens (12.2) and above
{$ELSE}
  {$MESSAGE ERROR 'This code requires  Delphi Athens (12.2) or later'}
{$IFEND}

{$IFNDEF WIN64}
  // Generates a compile-time error if the target platform is not Win64
  {$MESSAGE Error 'Unsupported platform'}
{$ENDIF}

{$Z4}  // Sets the enumeration size to 4 bytes
{$A8}  // Sets the alignment for record fields to 8 bytes

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}

{$WARN UNIT_PLATFORM OFF}
{$WARN UNIT_DEPRECATED OFF}

interface

{$REGION ' USES '}
uses
  WinApi.Windows,
  System.Math,
  System.Classes,
  System.IOUtils,
  System.AnsiStrings,
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  System.RTTI;
{$ENDREGION}

{$REGION ' CHANDRA '}
const
  /// <summary>
  /// The major version number of the Chandra library.
  /// </summary>
  /// <remarks>
  /// Indicates significant updates or breaking changes in the library's functionality.
  /// </remarks>
  CHANDRA_VERSION_MAJOR = '0';

  /// <summary>
  /// The minor version number of the Chandra library.
  /// </summary>
  /// <remarks>
  /// Represents incremental updates or additions of new features without breaking existing functionality.
  /// </remarks>
  CHANDRA_VERSION_MINOR = '1';

  /// <summary>
  /// The patch version number of the Chandra library.
  /// </summary>
  /// <remarks>
  /// Indicates small bug fixes or minor improvements to the library.
  /// </remarks>
  CHANDRA_VERSION_PATCH = '0';

  /// <summary>
  /// The full version string of the Chandra library.
  /// </summary>
  /// <remarks>
  /// Combines the major, minor, and patch versions into a single string in the format 'Major.Minor.Patch'.
  /// This provides a complete version identifier for the library.
  /// </remarks>
  CHANDRA_VERSION_FULL = CHANDRA_VERSION_MAJOR + '.' + CHANDRA_VERSION_MINOR + '.' + CHANDRA_VERSION_PATCH;

type
  /// <summary>
  /// Exception raised when an error occurs during registration in the Chandra library.
  /// </summary>
  /// <remarks>
  /// Typically used to indicate issues when registering routines or methods with the Lua state.
  /// </remarks>
  EChandraRegistrationError = class(Exception);

  /// <summary>
  /// General exception raised by the Chandra library.
  /// </summary>
  /// <remarks>
  /// This exception serves as the base class for other Chandra-specific exceptions.
  /// </remarks>
  EChandraException = class(Exception);

  /// <summary>
  /// Exception raised at runtime when the Lua script executes invalid operations or encounters unexpected states.
  /// </summary>
  /// <remarks>
  /// Commonly used to indicate runtime errors in the Lua script.
  /// </remarks>
  EChandraRuntimeException = class(Exception);

  /// <summary>
  /// Exception raised when the Lua script contains syntax errors.
  /// </summary>
  /// <remarks>
  /// This exception is typically thrown during the script loading or parsing phase.
  /// </remarks>
  EChandraSyntaxError = class(Exception);

  /// <summary>
  /// Wraps Delphi methods for Lua integration in the Chandra library.
  /// </summary>
  /// <remarks>
  /// Provides functionality to convert between Lua types and Delphi native types, enabling seamless method execution in Lua scripts.
  /// </remarks>
  TChandraMethodWrapper = class
  protected
    /// <summary>
    /// The RTTI metadata for the method being wrapped.
    /// </summary>
    FMethod: TRttiMethod;

    /// <summary>
    /// The RTTI context used for type information and metadata access.
    /// </summary>
    FContext: TRttiContext;

    /// <summary>
    /// The class containing the method being wrapped.
    /// </summary>
    FClass: TClass;

    /// <summary>
    /// Converts a Lua value on the stack to a Delphi native value.
    /// </summary>
    /// <param name="AState">Pointer to the Lua state.</param>
    /// <param name="AParamType">The expected Delphi parameter type.</param>
    /// <param name="AStackIndex">The index of the Lua stack value to convert.</param>
    /// <returns>The converted Delphi value.</returns>
    function ConvertLuaToNative(const AState: Pointer; const AParamType: TRttiType; const AStackIndex: Integer): TValue;

    /// <summary>
    /// Converts a Delphi native value to a Lua stack value.
    /// </summary>
    /// <param name="AState">Pointer to the Lua state.</param>
    /// <param name="AValue">The Delphi value to convert.</param>
    /// <returns>The number of Lua stack values pushed.</returns>
    function ConvertNativeToLua(const AState: Pointer; const AValue: TValue): Integer;
  public
    /// <summary>
    /// Constructs a method wrapper for a given method and class.
    /// </summary>
    /// <param name="AMethod">The RTTI method to wrap.</param>
    /// <param name="AClass">The class containing the method.</param>
    constructor Create(const AMethod: TRttiMethod; const AClass: TClass);

    /// <summary>
    /// Executes the wrapped method using the Lua stack as input.
    /// </summary>
    /// <param name="AState">Pointer to the Lua state.</param>
    /// <returns>The number of Lua stack values returned.</returns>
    function Execute(const AState: Pointer): Integer;
  end;

  /// <summary>
  /// The main class for integrating Lua scripts with Delphi code using the Chandra library.
  /// </summary>
  /// <remarks>
  /// Provides methods to register Delphi routines and classes with Lua, execute Lua scripts, and handle Lua state.
  /// </remarks>
  TChandra = class
  protected
    /// <summary>
    /// The RTTI context used for metadata access.
    /// </summary>
    FContext: TRttiContext;

    /// <summary>
    /// A dictionary mapping method names to their wrappers.
    /// </summary>
    FWrappers: TObjectDictionary<string, TChandraMethodWrapper>;

    /// <summary>
    /// Pointer to the Lua state.
    /// </summary>
    FState: Pointer;

    /// <summary>
    /// Garbage collection step.
    /// </summary>
    FGCStep: Integer;

    /// <summary>
    /// Registers a Delphi method with the Lua state.
    /// </summary>
    /// <param name="AMethod">The RTTI method to register.</param>
    /// <param name="AClass">The class containing the method.</param>
    procedure RegisterMethod(const AMethod: TRttiMethod; const AClass: TClass);

    /// <summary>
    /// Validates a Delphi method for Lua compatibility.
    /// </summary>
    /// <param name="AMethod">The RTTI method to validate.</param>
    procedure ValidateMethod(const AMethod: TRttiMethod);

    /// <summary>
    /// Pushes a Delphi value onto the Lua stack.
    /// </summary>
    /// <param name="AValue">The Delphi value to push.</param>
    /// <returns>True if the value was successfully pushed; otherwise, False.</returns>
    function PushValueToLua(const AValue: TValue): Boolean;

    /// <summary>
    /// Retrieves a Delphi value from the Lua stack.
    /// </summary>
    /// <param name="AStackIndex">The stack index of the Lua value.</param>
    /// <returns>The corresponding Delphi value.</returns>
    function GetValueFromLua(const AStackIndex: Integer): TValue;

    /// <summary>
    /// Converts a Delphi value to a Lua-compatible string representation.
    /// </summary>
    /// <param name="AValue">The Delphi value to convert, provided as a TVarRec.</param>
    /// <returns>A Lua-compatible string representation of the value.</returns>
    function LuaParamToString(const AValue: TVarRec): string;

    /// <summary>
    /// Pushes a pointer onto the Lua stack with optional type information.
    /// </summary>
    /// <param name="APtr">The pointer to push.</param>
    /// <param name="ATypeInfo">Optional type information for the pointer.</param>
    /// <returns>True if the pointer was successfully pushed; otherwise, False.</returns>
    function PushPointer(const APtr: Pointer; const ATypeInfo: PTypeInfo = nil): Boolean;

    /// <summary>
    /// Checks for Lua errors and raises an exception if an error is detected.
    /// </summary>
    /// <param name="AError">The Lua error code.</param>
    procedure CheckLuaError(const AError: Integer);

    /// <summary>
    /// Saves the compiled Lua bytecode to the specified stream.
    /// </summary>
    /// <param name="AStream">
    /// The stream where the bytecode will be saved. This can be a file stream, memory stream, etc.
    /// </param>
    procedure SaveByteCode(const AStream: TStream);

    /// <summary>
    /// Bundles a Lua script into a single output file.
    /// </summary>
    /// <param name="AInFilename">
    /// The input filename of the Lua script to bundle.
    /// </param>
    /// <param name="AOutFilename">
    /// The output filename where the bundled Lua script or bytecode will be saved.
    /// </param>
    procedure Bundle(const AInFilename: string; const AOutFilename: string);

  public
    /// <summary>
    /// Constructs a new instance of the TChandra class.
    /// </summary>
    constructor Create(); virtual;

    /// <summary>
    /// Destroys the instance of the TChandra class and releases resources.
    /// </summary>
    destructor Destroy(); override;

    /// <summary>
    /// Opens a new Lua state.
    /// </summary>
    /// <returns>True if the Lua state was successfully opened; otherwise, False.</returns>
    function Open(): Boolean;

    /// <summary>
    /// Closes the Lua state and releases associated resources.
    /// </summary>
    procedure Close();

    /// <summary>
    /// Resets the Lua state, clearing all loaded scripts and data.
    /// </summary>
    procedure Reset();

    /// <summary>
    /// Automatically registers all published methods of a given class with the Lua state.
    /// </summary>
    /// <param name="AClass">The class whose published methods are to be registered.</param>
    /// <param name="ATableName">
    /// Optional table name to group the methods in Lua. If provided, all registered methods will be accessible
    /// under this table in the Lua state (e.g., `TableName.MethodName`).
    /// </param>
    /// <remarks>
    /// This method inspects the provided class for all published methods and registers them in the Lua state.
    /// Only methods with parameters and a return type that map to basic Lua-compatible types are registered:
    /// <list type="bullet">
    /// <item>
    /// Parameters: `string`, `number` (Delphi `Integer` or `Double`), `Boolean`, and `Pointer`.
    /// </item>
    /// <item>
    /// Return: A single value of the above types.
    /// </item>
    /// Methods with unsupported parameter or return types are ignored.
    /// </remarks>
    /// <example>
    /// The following example demonstrates how to register a class's methods in Lua:
    /// <code>
    /// type
    ///   TMyClass = class
    ///   published
    ///     function Add(A, B: Double): Double;
    ///     function GetMessage(AName: string): string;
    ///   end;
    ///
    /// var
    ///   LChandra: TChandra;
    /// begin
    ///   LChandra := TChandra.Create;
    ///   try
    ///     LChandra.Open();
    ///     LChandra.RegisterRoutines(TMyClass, 'MyClass');
    ///     // In Lua, you can now call:
    ///     // local result = MyClass.Add(10, 20)
    ///     // local message = MyClass.GetMessage("John")
    ///   finally
    ///     LChandra.Free;
    ///   end;
    /// end;
    /// </code>
    /// </example>
    procedure RegisterRoutines(AClass: TClass; const ATableName: string = '');

    /// <summary>
    /// Loads and optionally executes a Lua script from a file.
    /// </summary>
    /// <param name="AFilename">The filename of the Lua script.</param>
    /// <param name="AAutoRun">Indicates whether to execute the script immediately after loading.</param>
    /// <returns>True if the script was successfully loaded; otherwise, False.</returns>
    function LoadFile(const AFilename: string; const AAutoRun: Boolean = True): Boolean;

    /// <summary>
    /// Loads and optionally executes a Lua script from a string.
    /// </summary>
    /// <param name="AData">The string containing the Lua script.</param>
    /// <param name="AAutoRun">Indicates whether to execute the script immediately after loading.</param>
    procedure LoadString(const AData: string; const AAutoRun: Boolean = True);

    /// <summary>
    /// Loads and optionally executes a Lua script from a memory buffer.
    /// </summary>
    /// <param name="AData">Pointer to the buffer containing the Lua script.</param>
    /// <param name="ASize">The size of the buffer in bytes.</param>
    /// <param name="AAutoRun">Indicates whether to execute the script immediately after loading.</param>
    procedure LoadBuffer(const AData: Pointer; const ASize: NativeUInt; const AAutoRun: Boolean = True);

    /// <summary>
    /// Checks if a Lua routine (function) exists in the Lua state.
    /// </summary>
    /// <param name="AName">The name of the Lua routine or construct to check. Supports nested constructs (e.g., `table.function`).</param>
    /// <returns>True if the routine exists and is callable; otherwise, False.</returns>
    function RoutineExist(const AName: string): Boolean;

    /// <summary>
    /// Checks if a Lua variable or nested field exists in the Lua state.
    /// </summary>
    /// <param name="AName">The name of the Lua variable or field to check. Supports nested constructs (e.g., `table.field`).</param>
    /// <returns>True if the variable exists; otherwise, False.</returns>
    function VariableExist(const AName: string): Boolean;

    /// <summary>
    /// Sets a Lua variable or nested field to a specified value in the Lua state.
    /// </summary>
    /// <param name="AName">The name of the Lua variable or field to set. Supports nested constructs (e.g., `table.field`).</param>
    /// <param name="AValue">The Delphi value to assign to the Lua variable or field.</param>
    procedure SetVariable(const AName: string; const AValue: TValue);

    /// <summary>
    /// Retrieves the value of a Lua variable or nested field from the Lua state.
    /// </summary>
    /// <param name="AName">The name of the Lua variable or field to retrieve. Supports nested constructs (e.g., `table.field`).</param>
    /// <returns>The value of the variable or field as a Delphi TValue.</returns>
    function GetVariable(const AName: string): TValue;

    /// <summary>
    /// Calls a Lua function by name with the specified parameters.
    /// </summary>
    /// <param name="AName">
    /// The name of the Lua function to call. Supports global functions, nested functions in tables, and any valid Lua construct
    /// (e.g., `globalFunction`, `Table.Function`, or `Module.SubModule.Function`).
    /// </param>
    /// <param name="AParams">
    /// An array of parameters to pass to the function. Only basic Lua-compatible types are supported:
    /// <list type="bullet">
    /// <item>
    /// `string`
    /// </item>
    /// <item>
    /// `number` (Delphi `Integer` or `Double`)
    /// </item>
    /// <item>
    /// `Boolean`
    /// </item>
    /// <item>
    /// `Pointer`
    /// </item>
    /// </list>
    /// Parameters that do not conform to these types will raise an exception.
    /// </param>
    /// <returns>
    /// The value returned by the Lua function, converted to a Delphi TValue. Only the following return types are supported:
    /// <list type="bullet">
    /// <item>
    /// `string`
    /// </item>
    /// <item>
    /// `number` (Delphi `Integer` or `Double`)
    /// </item>
    /// <item>
    /// `Boolean`
    /// </item>
    /// <item>
    /// `Pointer`
    /// </item>
    /// </list>
    /// If the Lua function does not return a value, the result will be an empty TValue.
    /// </returns>
    /// <remarks>
    /// The function dynamically pushes the parameters onto the Lua stack, calls the specified Lua function,
    /// and retrieves the return value. If the function is not found, or an error occurs during execution,
    /// an exception is raised.
    /// </remarks>
    /// <example>
    /// The following examples demonstrate calling Lua functions with supported parameter and return types:
    /// <code>
    /// // Lua code:
    /// // function Add(A, B) return A + B end
    /// // function Greet(Name) return "Hello, " .. Name end
    ///
    /// var
    ///   LChandra: TChandra;
    /// begin
    ///   LChandra := TChandra.Create;
    ///   try
    ///     LChandra.Open();
    ///     LChandra.LoadString('function Add(A, B) return A + B end');
    ///     LChandra.LoadString('function Greet(Name) return "Hello, " .. Name end');
    ///
    ///     // Call a Lua function with two numeric parameters
    ///     WriteLn(LChandra.Call('Add', [10, 20]).AsInteger); // Outputs: 30
    ///
    ///     // Call a Lua function with a string parameter
    ///     WriteLn(LChandra.Call('Greet', ['John']).AsString); // Outputs: Hello, John
    ///   finally
    ///     LChandra.Free;
    ///   end;
    /// end;
    /// </code>
    /// </example>
    function Call(const AName: string; const AParams: array of const): TValue;

    /// <summary>
    /// Compiles a Lua script to bytecode and writes the output to the specified stream.
    /// </summary>
    /// <param name="AFilename">
    /// The path to the Lua script file to be compiled.
    /// </param>
    /// <param name="AStream">
    /// The stream where the compiled bytecode will be written.
    /// </param>
    /// <param name="ACleanOutput">
    /// A boolean value indicating whether to clean up the temporary bundled file created during the compilation process.
    /// Set to <c>True</c> to delete the temporary file after compilation.
    /// Set to <c>False</c> to retain the temporary file for debugging or other purposes.
    /// </param>
    procedure CompileToStream(const AFilename: string; const AStream: TStream; const ACleanOutput: Boolean);

    /// <summary>
    /// Adds a search path to locate Lua scripts and resources.
    /// </summary>
    /// <param name="APath">
    /// The path to the directory to be added to the search paths.
    /// This can be a relative or absolute path.
    /// </param>
    procedure AddSearchPath(const APath: string);

    /// <summary>
    /// Outputs formatted text to the console without appending a newline at the end.
    /// </summary>
    /// <param name="AText">The format string, similar to the format specifiers used in <see cref="SysUtils.Format"/>.</param>
    /// <param name="AArgs">
    /// An array of values to be inserted into the placeholders defined in <paramref name="AText"/>.
    /// This allows for dynamic content in the output string.
    /// </param>
    /// <remarks>
    /// This method does not add a newline character at the end of the output, allowing for continuous text output.
    /// Use this for cases where appending more output on the same line is required.
    /// </remarks>
    /// <example>
    /// <code>
    /// Print('Hello, %s!', ['World']); // Output: Hello, World!
    /// </code>
    /// </example>
    procedure Print(const AText: string; const AArgs: array of const);

    /// <summary>
    /// Outputs formatted text to the console and appends a newline at the end.
    /// </summary>
    /// <param name="AText">The format string, similar to the format specifiers used in <see cref="SysUtils.Format"/>.</param>
    /// <param name="AArgs">
    /// An array of values to be inserted into the placeholders defined in <paramref name="AText"/>.
    /// This allows for dynamic content in the output string.
    /// </param>
    /// <remarks>
    /// This method appends a newline character at the end of the output, ensuring subsequent text
    /// appears on a new line. Use this for cases where individual lines of output are required.
    /// </remarks>
    /// <example>
    /// <code>
    /// PrintLn('Hello, %s!', ['World']); // Output: Hello, World! (with a newline)
    /// </code>
    /// </example>
    procedure PrintLn(const AText: string; const AArgs: array of const);

    /// <summary>
    /// Checks if a Lua payload (compiled bytecode) is currently stored and available for execution.
    /// </summary>
    /// <returns>
    /// <c>True</c> if a compiled Lua payload exists; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    /// This method verifies the presence of a previously stored Lua payload (compiled bytecode),
    /// ensuring that it can be executed or accessed. Use this method to conditionally execute or
    /// manage Lua payload operations.
    /// </remarks>
    /// <example>
    /// <code>
    /// if PayloadExist() then
    ///   WriteLn('Lua payload is ready.')
    /// else
    ///   WriteLn('No Lua payload found.');
    /// </code>
    /// </example>
    function PayloadExist(): Boolean;

    /// <summary>
    /// Compiles a Lua source file into bytecode and stores it as a payload embedded in the specified executable file.
    /// </summary>
    /// <param name="ASourceFilename">
    /// The source Lua file (.lua) containing the script to be compiled into bytecode.
    /// </param>
    /// <param name="AEXEFilename">
    /// The target executable file where the compiled Lua bytecode will be embedded as a payload.
    /// </param>
    /// <returns>
    /// <c>True</c> if the Lua payload was successfully compiled and stored; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    /// This method compiles the Lua source file specified by <paramref name="ASourceFilename"/>
    /// into bytecode and embeds it as a payload in the executable file specified by
    /// <paramref name="AEXEFilename"/>. Ensure that both file paths are valid and accessible
    /// before calling this method. A failed operation may indicate permission issues, invalid file
    /// formats, or errors during Lua compilation.
    /// </remarks>
    /// <example>
    /// <code>
    /// if StorePayload('script.lua', 'app.exe') then
    ///   WriteLn('Lua payload successfully stored.')
    /// else
    ///   WriteLn('Failed to store Lua payload.');
    /// </code>
    /// </example>
    function StorePayload(const ASourceFilename, AEXEFilename: string): Boolean;

    /// <summary>
    /// Executes the stored Lua payload (compiled bytecode), if available.
    /// </summary>
    /// <returns>
    /// <c>True</c> if the Lua payload was successfully executed; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    /// This method attempts to execute the previously stored Lua payload (compiled bytecode).
    /// Before calling this method, ensure that <see cref="PayloadExist"/> returns <c>True</c>.
    /// A failed execution may indicate issues with the stored Lua bytecode, runtime errors, or
    /// lack of permissions.
    /// </remarks>
    /// <example>
    /// <code>
    /// if PayloadExist() then
    /// begin
    ///   if RunPayload() then
    ///     WriteLn('Lua payload executed successfully.')
    ///   else
    ///     WriteLn('Failed to execute Lua payload.');
    /// end
    /// else
    ///   WriteLn('No Lua payload available.');
    /// </code>
    /// </example>
    function RunPayload(): Boolean;

    /// <summary>
    /// Event triggered before the reset operation is performed.
    /// </summary>
    /// <remarks>
    /// This method is a virtual placeholder designed to be overridden in descendant classes. It allows for
    /// custom pre-reset logic to be implemented. For example, you might use this to clean up resources,
    /// save state, or log information before a reset occurs.
    ///
    /// The base implementation does nothing, providing flexibility for descendants to define their specific behavior.
    /// </remarks>
    /// <example>
    /// <code>
    /// procedure TMyClass.OnBeforeReset();
    /// begin
    ///   // Custom logic before reset
    ///   WriteLn('Performing cleanup before reset...');
    /// end;
    /// </code>
    /// </example>
    procedure OnBeforeReset(); virtual;

    /// <summary>
    /// Event triggered after the reset operation is completed.
    /// </summary>
    /// <remarks>
    /// This method is a virtual placeholder designed to be overridden in descendant classes. It allows for
    /// custom post-reset logic to be implemented. For example, you might use this to reinitialize resources,
    /// notify other components, or log that the reset has been completed.
    ///
    /// The base implementation does nothing, providing flexibility for descendants to define their specific behavior.
    /// </remarks>
    /// <example>
    /// <code>
    /// procedure TMyClass.OnAfterReset();
    /// begin
    ///   // Custom logic after reset
    ///   WriteLn('Reinitialization completed after reset.');
    /// end;
    /// </code>
    /// </example>
    procedure OnAfterReset(); virtual;

    /// <summary>
    /// Sets the step size for Lua's garbage collector.
    /// </summary>
    /// <param name="AStep">The desired step size for garbage collection. Must be a positive integer.</param>
    procedure SetGCStepSize(const AStep: Integer);

    /// <summary>
    /// Retrieves the current step size of Lua's garbage collector.
    /// </summary>
    /// <returns>The current garbage collection step size as an integer.</returns>
    function GetGCStepSize(): Integer;

    /// <summary>
    /// Gets the amount of memory currently used by Lua's garbage collector.
    /// </summary>
    /// <returns>The memory usage of the garbage collector in bytes.</returns>
    function GetGCMemoryUsed(): Integer;

    /// <summary>
    /// Initiates a garbage collection cycle in Lua to free up unused memory.
    /// </summary>
    procedure CollectGarbage();

  end;

{$ENDREGION}

implementation

{$L Chandra.o}

{$REGION ' COMMON '}
var
  Marshaller: TMarshaller;

function EnableVirtualTerminalProcessing(): DWORD;
var
  HOut: THandle;
  LMode: DWORD;
begin
  HOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if HOut = INVALID_HANDLE_VALUE then
  begin
    Result := GetLastError;
    Exit;
  end;

  if not GetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if not SetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  Result := 0;  // Success
end;

function HasConsoleOutput: Boolean;
var
  Stdout: THandle;
begin
  Stdout := GetStdHandle(Std_Output_Handle);
  Win32Check(Stdout <> Invalid_Handle_Value);
  Result := Stdout <> 0;
end;

function TextToDelphiArray(const AInputText: string): string;
var
  I: Integer;
  LLineLength: Integer;
  LOutput: string;
begin
  LOutput := 'const cTEXT : array[1..' + IntToStr(Length(AInputText)) + '] of Byte = (' + sLineBreak;
  LLineLength := 0;

  // Loop through each character in the input text
  for I := 1 to Length(AInputText) do
  begin
    // Add the byte in hexadecimal format, but add the comma only if it's not the first element
    if I > 1 then
      if LLineLength > 0 then
        LOutput := LOutput + ', ';  // Add a comma only after the first byte

    LOutput := LOutput + '$' + IntToHex(Ord(AInputText[I]), 2);

    // Add a line break after 80 characters (including the comma and space)
    LLineLength := LLineLength + 5; // Each entry is 5 characters long ($xx, including comma)
    if LLineLength > 80-5 then
    begin
      LOutput := LOutput + ',' + sLineBreak;
      LLineLength := 0;
    end;
  end;

  // Add closing parenthesis and semicolon after the array
  LOutput := LOutput + sLineBreak + ');';
  Result := LOutput;
end;

procedure BinToCode();
begin
  TFile.WriteAllText('debugger.lua.txt', TextToDelphiArray(TFile.ReadAllText('debugger.lua')));
end;

function IsValidWin64PE(const AFilePath: string): Boolean;
var
  LFile: TFileStream;
  LDosHeader: TImageDosHeader;
  LPEHeaderOffset: DWORD;
  LPEHeaderSignature: DWORD;
  LFileHeader: TImageFileHeader;
begin
  Result := False;

  if not FileExists(AFilePath) then
    Exit;

  LFile := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // Check if file is large enough for DOS header
    if LFile.Size < SizeOf(TImageDosHeader) then
      Exit;

    // Read DOS header
    LFile.ReadBuffer(LDosHeader, SizeOf(TImageDosHeader));

    // Check DOS signature
    if LDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then // 'MZ'
      Exit;

      // Validate PE header offset
    LPEHeaderOffset := LDosHeader._lfanew;
    if LFile.Size < LPEHeaderOffset + SizeOf(DWORD) + SizeOf(TImageFileHeader) then
      Exit;

    // Seek to the PE header
    LFile.Position := LPEHeaderOffset;

    // Read and validate the PE signature
    LFile.ReadBuffer(LPEHeaderSignature, SizeOf(DWORD));
    if LPEHeaderSignature <> IMAGE_NT_SIGNATURE then // 'PE\0\0'
      Exit;

   // Read the file header
    LFile.ReadBuffer(LFileHeader, SizeOf(TImageFileHeader));

    // Check if it is a 64-bit executable
    if LFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then   Exit;

    // If all checks pass, it's a valid Win64 PE file
    Result := True;
  finally
    LFile.Free;
  end;
end;

function AddResFromMemory(const aModuleFile: string; const aName: string; aData: Pointer; aSize: Cardinal): Boolean;
var
  LHandle: THandle;
begin
  Result := False;
  if not TFile.Exists(aModuleFile) then Exit;
  LHandle := WinApi.Windows.BeginUpdateResourceW(PWideChar(aModuleFile), False);
  if LHandle <> 0 then
  begin
    WinApi.Windows.UpdateResourceW(LHandle, RT_RCDATA, PChar(aName), 1033 {ENGLISH, ENGLISH_US}, aData, aSize);
    Result := WinApi.Windows.EndUpdateResourceW(LHandle, False);
  end;
end;

function ResourceExists(aInstance: THandle; const aResName: string): Boolean;
begin
  Result := Boolean((FindResource(aInstance, PChar(aResName), RT_RCDATA) <> 0));
end;

function RemoveBOM(const AString: string): string; overload;
const
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
var
  LBytes: TBytes;
begin
  // Convert the input string to a byte array
  LBytes := TEncoding.UTF8.GetBytes(AString);

  // Check for UTF-8 BOM at the beginning
  if (Length(LBytes) >= 3) and
     (LBytes[0] = UTF8BOM[0]) and
     (LBytes[1] = UTF8BOM[1]) and
     (LBytes[2] = UTF8BOM[2]) then
  begin
    // Remove the BOM by copying the bytes after it
    Result := TEncoding.UTF8.GetString(LBytes, 3, Length(LBytes) - 3);
  end
  else
  begin
    // Return the original string if no BOM is detected
    Result := AString;
  end;
end;

function RemoveBOM(const ABytes: TBytes): TBytes; overload;
const
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16LEBOM: array[0..1] of Byte = ($FF, $FE);
  UTF16BEBOM: array[0..1] of Byte = ($FE, $FF);
var
  LStartIndex: Integer;
begin
  Result := ABytes;

  // Check for UTF-8 BOM
  if (Length(ABytes) >= 3) and
     (ABytes[0] = UTF8BOM[0]) and
     (ABytes[1] = UTF8BOM[1]) and
     (ABytes[2] = UTF8BOM[2]) then
  begin
    LStartIndex := 3; // Skip the UTF-8 BOM
  end
  // Check for UTF-16 LE BOM
  else if (Length(ABytes) >= 2) and
          (ABytes[0] = UTF16LEBOM[0]) and
          (ABytes[1] = UTF16LEBOM[1]) then
  begin
    LStartIndex := 2; // Skip the UTF-16 LE BOM
  end
  // Check for UTF-16 BE BOM
  else if (Length(ABytes) >= 2) and
          (ABytes[0] = UTF16BEBOM[0]) and
          (ABytes[1] = UTF16BEBOM[1]) then
  begin
    LStartIndex := 2; // Skip the UTF-16 BE BOM
  end
  else
  begin
    Exit; // No BOM found, return the original array
  end;

  // Create a new array without the BOM
  Result := Copy(ABytes, LStartIndex, Length(ABytes) - LStartIndex);
end;

function AsUTF8(const AText: string; const ARemoveBOM: Boolean=False): Pointer;
var
  LText: string;
begin
  if ARemoveBOM then
    LText := RemoveBOM(AText)
  else
    LText := AText;
  Result := Marshaller.AsUtf8(LText).ToPointer;
end;

{$ENDREGION}

{$REGION ' LUA API '}
const
  LUA_VERSION_MAJOR_N = 5;
  LUA_VERSION_MINOR_N = 5;
  LUA_VERSION_RELEASE_N = 0;
  LUA_VERSION_NUM = (LUA_VERSION_MAJOR_N*100+LUA_VERSION_MINOR_N);
  LUA_VERSION_RELEASE_NUM = (LUA_VERSION_NUM*100+LUA_VERSION_RELEASE_N);
  UINT_MAX = $ffffffff;
  LUAI_IS32INT = ((UINT_MAX shr 30)>=3);
  LUA_INT_INT = 1;
  LUA_INT_LONG = 2;
  LUA_INT_LONGLONG = 3;
  LUA_FLOAT_FLOAT = 1;
  LUA_FLOAT_DOUBLE = 2;
  LUA_FLOAT_LONGDOUBLE = 3;
  LUA_INT_DEFAULT = LUA_INT_LONGLONG;
  LUA_FLOAT_DEFAULT = LUA_FLOAT_DOUBLE;
  LUA_32BITS = 0;
  LUA_C89_NUMBERS = 0;
  LUA_INT_TYPE = LUA_INT_DEFAULT;
  LUA_FLOAT_TYPE = LUA_FLOAT_DEFAULT;
  LUA_PATH_SEP = ';';
  LUA_PATH_MARK = '?';
  LUA_EXEC_DIR = '!';
  LUA_LDIR = '!\lua\';
  LUA_CDIR = '!\';
  LUA_DIRSEP = '\';
  LUA_IGMARK = '-';
  LUA_NUMBER_FRMLEN = '';
  LUA_NUMBER_FMT = '%.15g';
  LUA_NUMBER_FMT_N = '%.17g';
  LUA_INTEGER_FRMLEN = 'll';
  LUA_INTEGER_FMT = '%' + LUA_INTEGER_FRMLEN + 'd';
  LLONG_MAX = 9223372036854775807;
  LUA_MAXINTEGER = LLONG_MAX;
  LLONG_MIN = (-9223372036854775807-1);
  LUA_MININTEGER = LLONG_MIN;
  ULLONG_MAX = $ffffffffffffffff;
  LUA_MAXUNSIGNED = ULLONG_MAX;
  LUAI_MAXSTACK = 1000000;
  LUA_IDSIZE = 60;
  LUA_SIGNATURE = #27'Lua';
  LUA_MULTRET = (-1);
  LUA_REGISTRYINDEX = (-LUAI_MAXSTACK-1000);
  LUA_OK = 0;
  LUA_YIELD = 1;
  LUA_ERRRUN = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM = 4;
  LUA_ERRERR = 5;
  LUA_TNONE = (-1);
  LUA_TNIL = 0;
  LUA_TBOOLEAN = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER = 3;
  LUA_TSTRING = 4;
  LUA_TTABLE = 5;
  LUA_TFUNCTION = 6;
  LUA_TUSERDATA = 7;
  LUA_TTHREAD = 8;
  LUA_NUMTYPES = 9;
  LUA_MINSTACK = 20;
  LUA_RIDX_GLOBALS = 2;
  LUA_RIDX_MAINTHREAD = 3;
  LUA_RIDX_LAST = 3;
  LUA_OPADD = 0;
  LUA_OPSUB = 1;
  LUA_OPMUL = 2;
  LUA_OPMOD = 3;
  LUA_OPPOW = 4;
  LUA_OPDIV = 5;
  LUA_OPIDIV = 6;
  LUA_OPBAND = 7;
  LUA_OPBOR = 8;
  LUA_OPBXOR = 9;
  LUA_OPSHL = 10;
  LUA_OPSHR = 11;
  LUA_OPUNM = 12;
  LUA_OPBNOT = 13;
  LUA_OPEQ = 0;
  LUA_OPLT = 1;
  LUA_OPLE = 2;
  LUA_GCSTOP = 0;
  LUA_GCRESTART = 1;
  LUA_GCCOLLECT = 2;
  LUA_GCCOUNT = 3;
  LUA_GCCOUNTB = 4;
  LUA_GCSTEP = 5;
  LUA_GCISRUNNING = 6;
  LUA_GCGEN = 7;
  LUA_GCINC = 8;
  LUA_GCPARAM = 9;
  LUA_GCPMINORMUL = 0;
  LUA_GCPMAJORMINOR = 1;
  LUA_GCPMINORMAJOR = 2;
  LUA_GCPPAUSE = 3;
  LUA_GCPSTEPMUL = 4;
  LUA_GCPSTEPSIZE = 5;
  LUA_GCPN = 6;
  LUA_N2SBUFFSZ = 64;
  LUA_HOOKCALL = 0;
  LUA_HOOKRET = 1;
  LUA_HOOKLINE = 2;
  LUA_HOOKCOUNT = 3;
  LUA_HOOKTAILCALL = 4;
  LUA_MASKCALL = (1 shl LUA_HOOKCALL);
  LUA_MASKRET = (1 shl LUA_HOOKRET);
  LUA_MASKLINE = (1 shl LUA_HOOKLINE);
  LUA_MASKCOUNT = (1 shl LUA_HOOKCOUNT);
  LUA_GLIBK = 1;
  LUA_LOADLIBNAME = 'package';
  LUA_LOADLIBK = (LUA_GLIBK shl 1);
  LUA_COLIBNAME = 'coroutine';
  LUA_COLIBK = (LUA_LOADLIBK shl 1);
  LUA_DBLIBNAME = 'debug';
  LUA_DBLIBK = (LUA_COLIBK shl 1);
  LUA_IOLIBNAME = 'io';
  LUA_IOLIBK = (LUA_DBLIBK shl 1);
  LUA_MATHLIBNAME = 'math';
  LUA_MATHLIBK = (LUA_IOLIBK shl 1);
  LUA_OSLIBNAME = 'os';
  LUA_OSLIBK = (LUA_MATHLIBK shl 1);
  LUA_STRLIBNAME = 'string';
  LUA_STRLIBK = (LUA_OSLIBK shl 1);
  LUA_TABLIBNAME = 'table';
  LUA_TABLIBK = (LUA_STRLIBK shl 1);
  LUA_UTF8LIBNAME = 'utf8';
  LUA_UTF8LIBK = (LUA_TABLIBK shl 1);
  LUA_GNAME = '_G';
  LUA_ERRFILE = (LUA_ERRERR+1);
  LUA_LOADED_TABLE = '_LOADED';
  LUA_PRELOAD_TABLE = '_PRELOAD';
  LUA_NOREF = (-2);
  LUA_REFNIL = (-1);
  LUA_FILEHANDLE = 'FILE*';

type
  // Forward declarations
  PPUTF8Char = ^PUTF8Char;
  PNativeUInt = ^NativeUInt;
  PPointer = ^Pointer;
  PCallInfo = Pointer;
  PPCallInfo = ^PCallInfo;
  Plua_Debug = ^lua_Debug;
  PluaL_Reg = ^luaL_Reg;
  PluaL_Buffer = ^luaL_Buffer;
  PluaL_Stream = ^luaL_Stream;

  Plua_State = Pointer;
  PPlua_State = ^Plua_State;
  lua_Number = Double;
  lua_Integer = Int64;
  lua_Unsigned = UInt64;
  lua_KContext = NativeInt;

  lua_CFunction = function(L: Plua_State): Integer; cdecl;
  lua_KFunction = function(L: Plua_State; status: Integer; ctx: lua_KContext): Integer; cdecl;
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: PNativeUInt): PUTF8Char; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: NativeUInt; ud: Pointer): Integer; cdecl;
  lua_Alloc = function(ud: Pointer; ptr: Pointer; osize: NativeUInt; nsize: NativeUInt): Pointer; cdecl;
  lua_WarnFunction = procedure(ud: Pointer; const msg: PUTF8Char; tocont: Integer); cdecl;
  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

  lua_Debug = record
    event: Integer;
    name: PUTF8Char;
    namewhat: PUTF8Char;
    what: PUTF8Char;
    source: PUTF8Char;
    srclen: NativeUInt;
    currentline: Integer;
    linedefined: Integer;
    lastlinedefined: Integer;
    nups: Byte;
    nparams: Byte;
    isvararg: UTF8Char;
    extraargs: Byte;
    istailcall: UTF8Char;
    ftransfer: Integer;
    ntransfer: Integer;
    short_src: array [0..59] of UTF8Char;
    i_ci: PCallInfo;
  end;

  luaL_Reg = record
    name: PUTF8Char;
    func: lua_CFunction;
  end;

  P_anonymous_type_1 = ^_anonymous_type_1;
  _anonymous_type_1 = record
    case Integer of
      0: (n: lua_Number);
      1: (u: Double);
      2: (s: Pointer);
      3: (i: lua_Integer);
      4: (l: Longint);
      5: (b: array [0..1023] of UTF8Char);
  end;

  luaL_Buffer = record
    b: PUTF8Char;
    size: NativeUInt;
    n: NativeUInt;
    L: Plua_State;
    init: _anonymous_type_1;
  end;

  luaL_Stream = record
    f: PPointer;
    closef: lua_CFunction;
  end;

function  lua_newstate(f: lua_Alloc; ud: Pointer; seed: Cardinal): Plua_State; cdecl; external;
procedure lua_close(L: Plua_State); cdecl; external;
function  lua_newthread(L: Plua_State): Plua_State; cdecl; external;
function  lua_closethread(L: Plua_State; from: Plua_State): Integer; cdecl; external;
function  lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external;
function  lua_version(L: Plua_State): lua_Number; cdecl; external;
function  lua_absindex(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_gettop(L: Plua_State): Integer; cdecl; external;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external;
procedure lua_pushvalue(L: Plua_State; idx: Integer); cdecl; external;
procedure lua_rotate(L: Plua_State; idx: Integer; n: Integer); cdecl; external;
procedure lua_copy(L: Plua_State; fromidx: Integer; toidx: Integer); cdecl; external;
function  lua_checkstack(L: Plua_State; n: Integer): Integer; cdecl; external;
procedure lua_xmove(from: Plua_State; &to: Plua_State; n: Integer); cdecl; external;
function  lua_isnumber(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_isstring(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_iscfunction(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_isinteger(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_isuserdata(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_typename(L: Plua_State; tp: Integer): PUTF8Char; cdecl; external;
function  lua_tonumberx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Number; cdecl; external;
function  lua_tointegerx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Integer; cdecl; external;
function  lua_toboolean(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_tolstring(L: Plua_State; idx: Integer; len: PNativeUInt): PUTF8Char; cdecl; external;
function  lua_rawlen(L: Plua_State; idx: Integer): lua_Unsigned; cdecl; external;
function  lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl; external;
function  lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl; external;
function  lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl; external;
function  lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl; external;
procedure lua_arith(L: Plua_State; op: Integer); cdecl; external;
function  lua_rawequal(L: Plua_State; idx1: Integer; idx2: Integer): Integer; cdecl; external;
function  lua_compare(L: Plua_State; idx1: Integer; idx2: Integer; op: Integer): Integer; cdecl; external;
procedure lua_pushnil(L: Plua_State); cdecl; external;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl; external;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl; external;
function  lua_pushlstring(L: Plua_State; const s: PUTF8Char; len: NativeUInt): PUTF8Char; cdecl; external;
function  lua_pushextlstring(L: Plua_State; const s: PUTF8Char; len: NativeUInt; falloc: lua_Alloc; ud: Pointer): PUTF8Char; cdecl; external;
function  lua_pushstring(L: Plua_State; const s: PUTF8Char): PUTF8Char; cdecl; external;
function  lua_pushvfstring(L: Plua_State; const fmt: PUTF8Char; argp: Pointer): PUTF8Char; cdecl; external;
function  lua_pushfstring(L: Plua_State; const fmt: PUTF8Char): PUTF8Char varargs; cdecl; external;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl; external;
procedure lua_pushboolean(L: Plua_State; b: Integer); cdecl; external;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl; external;
function  lua_pushthread(L: Plua_State): Integer; cdecl; external;
function  lua_getglobal(L: Plua_State; const name: PUTF8Char): Integer; cdecl; external;
function  lua_gettable(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_getfield(L: Plua_State; idx: Integer; const k: PUTF8Char): Integer; cdecl; external;
function  lua_geti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl; external;
function  lua_rawget(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_rawgeti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl; external;
function  lua_rawgetp(L: Plua_State; idx: Integer; const p: Pointer): Integer; cdecl; external;
procedure lua_createtable(L: Plua_State; narr: Cardinal; nrec: Cardinal); cdecl; external;
function  lua_newuserdatauv(L: Plua_State; sz: NativeUInt; nuvalue: Integer): Pointer; cdecl; external;
function  lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external;
function  lua_getiuservalue(L: Plua_State; idx: Integer; n: Integer): Integer; cdecl; external;
procedure lua_setglobal(L: Plua_State; const name: PUTF8Char); cdecl; external;
procedure lua_settable(L: Plua_State; idx: Integer); cdecl; external;
procedure lua_setfield(L: Plua_State; idx: Integer; const k: PUTF8Char); cdecl; external;
procedure lua_seti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl; external;
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external;
procedure lua_rawseti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl; external;
procedure lua_rawsetp(L: Plua_State; idx: Integer; const p: Pointer); cdecl; external;
function  lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external;
function  lua_setiuservalue(L: Plua_State; idx: Integer; n: Integer): Integer; cdecl; external;
procedure lua_callk(L: Plua_State; nargs: Integer; nresults: Integer; ctx: lua_KContext; k: lua_KFunction); cdecl; external;
function  lua_pcallk(L: Plua_State; nargs: Integer; nresults: Integer; errfunc: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external;
function  lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PUTF8Char; const mode: PUTF8Char): Integer; cdecl; external;
function  lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: Integer): Integer; cdecl; external;
function  lua_yieldk(L: Plua_State; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external;
function  lua_resume(L: Plua_State; from: Plua_State; narg: Integer; nres: PInteger): Integer; cdecl; external;
function  lua_status(L: Plua_State): Integer; cdecl; external;
function  lua_isyieldable(L: Plua_State): Integer; cdecl; external;
procedure lua_setwarnf(L: Plua_State; f: lua_WarnFunction; ud: Pointer); cdecl; external;
procedure lua_warning(L: Plua_State; const msg: PUTF8Char; tocont: Integer); cdecl; external;
function  lua_gc(L: Plua_State; what: Integer): Integer varargs; cdecl; external;
function  lua_error(L: Plua_State): Integer; cdecl; external;
function  lua_next(L: Plua_State; idx: Integer): Integer; cdecl; external;
procedure lua_concat(L: Plua_State; n: Integer); cdecl; external;
procedure lua_len(L: Plua_State; idx: Integer); cdecl; external;
function  lua_numbertostrbuff(L: Plua_State; idx: Integer; buff: PUTF8Char): Cardinal; cdecl; external;
function  lua_stringtonumber(L: Plua_State; const s: PUTF8Char): NativeUInt; cdecl; external;
function  lua_getallocf(L: Plua_State; ud: PPointer): lua_Alloc; cdecl; external;
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl; external;
procedure lua_toclose(L: Plua_State; idx: Integer); cdecl; external;
procedure lua_closeslot(L: Plua_State; idx: Integer); cdecl; external;
function  lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl; external;
function  lua_getinfo(L: Plua_State; const what: PUTF8Char; ar: Plua_Debug): Integer; cdecl; external;
function  lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PUTF8Char; cdecl; external;
function  lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PUTF8Char; cdecl; external;
function  lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PUTF8Char; cdecl; external;
function  lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PUTF8Char; cdecl; external;
function  lua_upvalueid(L: Plua_State; fidx: Integer; n: Integer): Pointer; cdecl; external;
procedure lua_upvaluejoin(L: Plua_State; fidx1: Integer; n1: Integer; fidx2: Integer; n2: Integer); cdecl; external;
procedure lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer); cdecl; external;
function  lua_gethook(L: Plua_State): lua_Hook; cdecl; external;
function  lua_gethookmask(L: Plua_State): Integer; cdecl; external;
function  lua_gethookcount(L: Plua_State): Integer; cdecl; external;
function  luaopen_base(L: Plua_State): Integer; cdecl; external;
function  luaopen_package(L: Plua_State): Integer; cdecl; external;
function  luaopen_coroutine(L: Plua_State): Integer; cdecl; external;
function  luaopen_debug(L: Plua_State): Integer; cdecl; external;
function  luaopen_io(L: Plua_State): Integer; cdecl; external;
function  luaopen_math(L: Plua_State): Integer; cdecl; external;
function  luaopen_os(L: Plua_State): Integer; cdecl; external;
function  luaopen_string(L: Plua_State): Integer; cdecl; external;
function  luaopen_table(L: Plua_State): Integer; cdecl; external;
function  luaopen_utf8(L: Plua_State): Integer; cdecl; external;
procedure luaL_openselectedlibs(L: Plua_State; load: Integer; preload: Integer); cdecl; external;
procedure luaL_checkversion_(L: Plua_State; ver: lua_Number; sz: NativeUInt); cdecl; external;
function  luaL_getmetafield(L: Plua_State; obj: Integer; const e: PUTF8Char): Integer; cdecl; external;
function  luaL_callmeta(L: Plua_State; obj: Integer; const e: PUTF8Char): Integer; cdecl; external;
function  luaL_tolstring(L: Plua_State; idx: Integer; len: PNativeUInt): PUTF8Char; cdecl; external;
function  luaL_argerror(L: Plua_State; arg: Integer; const extramsg: PUTF8Char): Integer; cdecl; external;
function  luaL_typeerror(L: Plua_State; arg: Integer; const tname: PUTF8Char): Integer; cdecl; external;
function   luaL_checklstring(L: Plua_State; arg: Integer; l_: PNativeUInt): PUTF8Char; cdecl; external;
function   luaL_optlstring(L: Plua_State; arg: Integer; const def: PUTF8Char; l_: PNativeUInt): PUTF8Char; cdecl; external;
function  luaL_checknumber(L: Plua_State; arg: Integer): lua_Number; cdecl; external;
function  luaL_optnumber(L: Plua_State; arg: Integer; def: lua_Number): lua_Number; cdecl; external;
function  luaL_checkinteger(L: Plua_State; arg: Integer): lua_Integer; cdecl; external;
function  luaL_optinteger(L: Plua_State; arg: Integer; def: lua_Integer): lua_Integer; cdecl; external;
procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PUTF8Char); cdecl; external;
procedure luaL_checktype(L: Plua_State; arg: Integer; t: Integer); cdecl; external;
procedure luaL_checkany(L: Plua_State; arg: Integer); cdecl; external;
function  luaL_newmetatable(L: Plua_State; const tname: PUTF8Char): Integer; cdecl; external;
procedure luaL_setmetatable(L: Plua_State; const tname: PUTF8Char); cdecl; external;
function  luaL_testudata(L: Plua_State; ud: Integer; const tname: PUTF8Char): Pointer; cdecl; external;
function  luaL_checkudata(L: Plua_State; ud: Integer; const tname: PUTF8Char): Pointer; cdecl; external;
procedure luaL_where(L: Plua_State; lvl: Integer); cdecl; external;
function  luaL_error(L: Plua_State; const fmt: PUTF8Char): Integer varargs; cdecl; external;
function  luaL_checkoption(L: Plua_State; arg: Integer; const def: PUTF8Char; lst: PPUTF8Char): Integer; cdecl; external;
function  luaL_fileresult(L: Plua_State; stat: Integer; const fname: PUTF8Char): Integer; cdecl; external;
function  luaL_execresult(L: Plua_State; stat: Integer): Integer; cdecl; external;
function  luaL_ref(L: Plua_State; t: Integer): Integer; cdecl; external;
procedure luaL_unref(L: Plua_State; t: Integer; ref: Integer); cdecl; external;
function  luaL_loadfilex(L: Plua_State; const filename: PUTF8Char; const mode: PUTF8Char): Integer; cdecl; external;
function  luaL_loadbufferx(L: Plua_State; const buff: PUTF8Char; sz: NativeUInt; const name: PUTF8Char; const mode: PUTF8Char): Integer; cdecl; external;
function  luaL_loadstring(L: Plua_State; const s: PUTF8Char): Integer; cdecl; external;
function  luaL_newstate(): Plua_State; cdecl; external;
function  luaL_makeseed(L: Plua_State): Cardinal; cdecl; external;
function  luaL_len(L: Plua_State; idx: Integer): lua_Integer; cdecl; external;
procedure luaL_addgsub(b: PluaL_Buffer; const s: PUTF8Char; const p: PUTF8Char; const r: PUTF8Char); cdecl; external;
function  luaL_gsub(L: Plua_State; const s: PUTF8Char; const p: PUTF8Char; const r: PUTF8Char): PUTF8Char; cdecl; external;
procedure luaL_setfuncs(L: Plua_State; const l_: PluaL_Reg; nup: Integer); cdecl; external;
function  luaL_getsubtable(L: Plua_State; idx: Integer; const fname: PUTF8Char): Integer; cdecl; external;
procedure luaL_traceback(L: Plua_State; L1: Plua_State; const msg: PUTF8Char; level: Integer); cdecl; external;
procedure luaL_requiref(L: Plua_State; const modname: PUTF8Char; openf: lua_CFunction; glb: Integer); cdecl; external;
procedure luaL_buffinit(L: Plua_State; B: PluaL_Buffer); cdecl; external;
function  luaL_prepbuffsize(B: PluaL_Buffer; sz: NativeUInt): PUTF8Char; cdecl; external;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PUTF8Char; l: NativeUInt); cdecl; external;
procedure luaL_addstring(B: PluaL_Buffer; const s: PUTF8Char); cdecl; external;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl; external;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl; external;
procedure luaL_pushresultsize(B: PluaL_Buffer; sz: NativeUInt); cdecl; external;
function  luaL_buffinitsize(L: Plua_State; B: PluaL_Buffer; sz: NativeUInt): PUTF8Char; cdecl; external;

procedure luaL_openlibs(L: Pointer); inline;
const
  LUA_ALL_LIBS = not 0;
begin
  luaL_openselectedlibs(L, LUA_ALL_LIBS, 0);
end;

function lua_isfunction(aState: Pointer; n: Integer): Boolean;
begin
  Result := Boolean(lua_type(aState, n) = LUA_TFUNCTION);
end;

function lua_isvariable(aState: Pointer; n: Integer): Boolean;
var
  aType: Integer;
begin
  aType := lua_type(aState, n);

  if (aType = LUA_TBOOLEAN) or (aType = LUA_TLIGHTUSERDATA) or (aType = LUA_TNUMBER) or (aType = LUA_TSTRING) then
    Result := True
  else
    Result := False;
end;

procedure lua_newtable(aState: Pointer);
begin
  lua_createtable(aState, 0, 0);
end;

procedure lua_pop(aState: Pointer; n: Integer);
begin
  lua_settop(aState, -n - 1);
end;

procedure lua_pushcfunction(aState: Pointer; aFunc: Lua_CFunction);
begin
  lua_pushcclosure(aState, aFunc, 0);
end;

procedure lua_register(aState: Pointer; aName: PAnsiChar; aFunc: Lua_CFunction);
begin
  lua_pushcfunction(aState, aFunc);
  lua_setglobal(aState, aName);
end;

function lua_isnil(aState: Pointer; n: Integer): Boolean;
begin
  Result := Boolean(lua_type(aState, n) = LUA_TNIL);
end;

function lua_tostring(aState: Pointer; idx: Integer): string;
begin
  Result := string(lua_tolstring(aState, idx, nil));
end;

function lua_pcall(L: Pointer; N, R, F: Integer): Integer;
begin
  Result := lua_pcallk(L, N, R, F, 0, nil);
end;

function luaL_loadfile(L: Pointer; const F: PAnsiChar): Integer;
begin
  Result := luaL_loadfilex(L, F, nil);
end;

function luaL_dofile(aState: Pointer; aFilename: PAnsiChar): Integer;
Var
  Res: Integer;
begin
  Res := luaL_loadfile(aState, aFilename);
  if Res = 0 then
    Res := lua_pcall(aState, 0, 0, 0);
  Result := Res;
end;

function luaL_dostring(aState: Pointer; aStr: PAnsiChar): Integer;
Var
  Res: Integer;
begin
  Res := luaL_loadstring(aState, aStr);
  if Res = 0 then
    Res := lua_pcall(aState, 0, 0, 0);
  Result := Res;
end;

function luaL_loadbuffer(L: Pointer; const S: PAnsiChar; Sz: NativeUInt; const N: PAnsiChar): Integer;
begin
  Result := luaL_loadbufferx(L, S, Sz, N, nil);
end;

function luaL_dobuffer(aState: Pointer; aBuffer: Pointer; aSize: NativeUInt;
  aName: PAnsiChar): Integer; inline;
var
  Res: Integer;
begin
  Res := luaL_loadbuffer(aState, aBuffer, aSize, aName);
  if Res = 0 then
    Res := lua_pcall(aState, 0, 0, 0);
  Result := Res;
end;

function lua_upvalueindex(i: Integer): Integer; inline;
begin
  Result := LUA_REGISTRYINDEX - i; // Updated to reflect modern API usage
end;

function lua_newuserdata(L: Pointer; S: NativeUInt): Pointer;
begin
  Result := lua_newuserdatauv(L, S, 1);
end;

function lua_tonumber(L: Plua_State; AIndex: Integer): lua_Number;
begin
  Result := lua_tonumberx(L, AIndex, nil);
end;

function lua_tointeger(L: Plua_State; AIndex: Integer): lua_Integer;
begin
  Result := lua_tointegerx(L, AIndex, nil);
end;

function lua_istable(L: Plua_State; AIndex: Integer): Boolean;
begin
  Result := lua_type(L, AIndex) = LUA_TTABLE;
end;

function luaL_checkstring(L: Plua_State; AIndex: Integer): PAnsiChar;
begin
  Result := luaL_checklstring(L, AIndex, nil);
end;

procedure lua_insert(L: Plua_State; AIndex: Integer);
begin
  lua_rotate(L, AIndex, 1);
end;

procedure lua_remove(L: Plua_State; AIndex: Integer);
begin
  lua_rotate(L, AIndex, -1);
  lua_pop(L, 1);
end;


function luaL_getmetatable(L: Plua_State; const AName: PAnsiChar): Integer;
begin
  Result := lua_getfield(L, LUA_REGISTRYINDEX, AName);
end;

{$REGION ' LUA DEBUGGER '}
const
  DEBUGGER_LUA =
'''
--[[---------------------------------------------------------------------------
Acknowledgment:
   This code is based on the original debugger.lua project by
   slembcke, available at:
     https://github.com/slembcke/debugger.lua
   Credit goes to the original developer for their foundational work, which
   this unit builds upon.
-----------------------------------------------------------------------------]]

local dbg = {}

-- ANSI Colors
local COLOR_GRAY = string.char(27) .. "[90m"
local COLOR_RED = string.char(27) .. "[91m"
local COLOR_BLUE = string.char(27) .. "[94m"
local COLOR_YELLOW = string.char(27) .. "[33m"
local COLOR_RESET = string.char(27) .. "[0m"
local GREEN_CARET = string.char(27) .. "[92m => " .. COLOR_RESET

-- Check for Windows
local function is_windows()
    return package.config:sub(1,1) == '\\'
end

-- Check if colors are supported
local function supports_colors()
    if is_windows() then
        -- Windows 10+ supports ANSI colors
        local version = os.getenv("WINVER") or os.getenv("VERSION")
        return version ~= nil
    else
        -- Unix-like systems
        return os.getenv("TERM") and os.getenv("TERM") ~= "dumb"
    end
end

-- Disable colors if terminal doesn't support them
if not supports_colors then
    COLOR_GRAY = ""
    COLOR_RED = ""
    COLOR_BLUE = ""
    COLOR_YELLOW = ""
    COLOR_RESET = ""
    GREEN_CARET = " => "
end

-- State tracking
local current_frame = 0
local step_mode = nil
local current_func = nil
local last_cmd = "h"  -- Move last_cmd to file scope

-- Source cache
local source_cache = {}

local function pretty(obj, max_depth)
    max_depth = max_depth or 3
    local function pp(obj, depth)
        if depth > max_depth then return tostring(obj) end
        if type(obj) == "string" then return string.format("%q", obj) end
        if type(obj) ~= "table" then return tostring(obj) end
        local mt = getmetatable(obj)
        if mt and mt.__tostring then return tostring(obj) end

        local parts = {}
        for k, v in pairs(obj) do
            local key = type(k) == "string" and k or "[" .. pp(k, depth) .. "]"
            table.insert(parts, key .. " = " .. pp(v, depth + 1))
        end
        return "{" .. table.concat(parts, ", ") .. "}"
    end
    return pp(obj, 1)
end

local function get_locals(level)
    local vars = {}
    local i = 1
    while true do
        local name, value = debug.getlocal(level, i)
        if not name then break end
        if name:sub(1, 1) ~= "(" then  -- Skip internal variables
            vars[name] = value
        end
        i = i + 1
    end
    return vars
end

local function get_upvalues(func)
    local vars = {}
    local i = 1
    while true do
        local name, value = debug.getupvalue(func, i)
        if not name then break end
        vars[name] = value
        i = i + 1
    end
    return vars
end

local function get_source_lines(info)
    if source_cache[info.source] then
        return source_cache[info.source]
    end

    local lines = {}
    if info.source:sub(1, 1) == "@" then
        local file = io.open(info.source:sub(2))
        if file then
            for line in file:lines() do
                table.insert(lines, line)
            end
            file:close()
        end
    else
        for line in info.source:gmatch("[^\n]+") do
            table.insert(lines, line)
        end
    end
    source_cache[info.source] = lines
    return lines
end

local function get_short_src(source)
    if source:sub(1, 1) == "@" then
        return source:sub(2)  -- Remove @ prefix
    end
    -- For non-file sources, return just "[string]"
    return "[string]"
end

local function print_break_location(info, reason)
    reason = reason or "dbg()"
    local short_src = get_short_src(info.source)
    local prefix = reason and (COLOR_YELLOW .. "break via " .. COLOR_RED .. reason .. GREEN_CARET) or ""
    print(string.format("%s%s%s:%s%d%s in %s",
        prefix,
        COLOR_BLUE, short_src,
        COLOR_YELLOW, info.currentline,
        COLOR_RESET,
        info.name or "main chunk"
    ))
end

local function print_frame_source(info, context_lines)
    context_lines = context_lines or 2
    local lines = get_source_lines(info)
    if not lines then return end

    local line_num = info.currentline
    for i = math.max(1, line_num - context_lines),
             math.min(#lines, line_num + context_lines) do
        local marker = i == line_num and GREEN_CARET or "    "
        print(string.format(COLOR_GRAY .. "% 4d%s%s",
            i, marker, lines[i] .. COLOR_RESET))
    end
end

local function evaluate_expression(expr, level)
    if not expr or expr == "" then
        print(COLOR_RED .. "Usage: p <expression>" .. COLOR_RESET)
        return
    end

    local locals = get_locals(level)
    local info = debug.getinfo(level, "f")
    local upvalues = get_upvalues(info.func)

    -- Create environment with locals, upvalues, and globals
    local env = setmetatable(locals, {__index = _G})
    for k, v in pairs(upvalues) do env[k] = v end

    local chunk, err = load("return " .. expr, "=expr", "t", env)
    if not chunk then
        print(COLOR_RED .. "Error: " .. err .. COLOR_RESET)
        return
    end

    local success, result = pcall(chunk)
    if not success then
        print(COLOR_RED .. "Error: " .. result .. COLOR_RESET)
        return
    end

    print(COLOR_BLUE .. expr .. GREEN_CARET .. pretty(result))
end

local function print_locals(level)
    local locals = get_locals(level)
    local info = debug.getinfo(level, "f")
    local upvalues = get_upvalues(info.func)

    print(COLOR_BLUE .. "Local variables:" .. COLOR_RESET)
    local sorted_locals = {}
    for name, value in pairs(locals) do
        table.insert(sorted_locals, {name = name, value = value})
    end
    table.sort(sorted_locals, function(a, b) return a.name < b.name end)

    for _, var in ipairs(sorted_locals) do
        print(string.format("  %s = %s", var.name, pretty(var.value)))
    end

    if next(upvalues) then
        print(COLOR_BLUE .. "\nUpvalues:" .. COLOR_RESET)
        local sorted_upvalues = {}
        for name, value in pairs(upvalues) do
            table.insert(sorted_upvalues, {name = name, value = value})
        end
        table.sort(sorted_upvalues, function(a, b) return a.name < b.name end)

        for _, var in ipairs(sorted_upvalues) do
            print(string.format("  %s = %s", var.name, pretty(var.value)))
        end
    end
end

local function print_help()
    local help = {
        {cmd = "<return>", desc = "re-run last command"},
        {cmd = "c(ontinue)", desc = "continue execution"},
        {cmd = "s(tep)", desc = "step forward by one line (into functions)"},
        {cmd = "n(ext)", desc = "step forward by one line (skipping over functions)"},
        {cmd = "f(inish)", desc = "step forward until exiting the current function"},
        {cmd = "u(p)", desc = "move up the stack by one frame"},
        {cmd = "d(own)", desc = "move down the stack by one frame"},
        {cmd = "w(here) [count]", desc = "print source code around the current line"},
        {cmd = "p(rint) [expr]", desc = "evaluate expression and print the result"},
        {cmd = "t(race)", desc = "print the stack trace"},
        {cmd = "l(ocals)", desc = "print the function arguments, locals and upvalues"},
        {cmd = "h(elp)", desc = "print this message"},
        {cmd = "q(uit)", desc = "halt execution"},
    }

    for _, item in ipairs(help) do
        print(string.format("%s%s%s%s%s",
            COLOR_BLUE, item.cmd,
            COLOR_YELLOW, GREEN_CARET, item.desc))
    end
end

local function print_stack_trace()
    local level = 1
    print(COLOR_BLUE .. "Stack trace:" .. COLOR_RESET)
    while true do
        local info = debug.getinfo(level, "Snl")
        if not info then break end

        local is_current = level == current_frame + 2
        local marker = is_current and GREEN_CARET or "    "
        local name = info.name or "<unknown>"
        local source = get_short_src(info.source)

        print(string.format(COLOR_GRAY .. "% 4d%s%s:%d in %s",
            level - 1, marker, source, info.currentline, name))

        level = level + 1
    end
end

-- Debug hook
local function debug_hook(event, line)

    if event ~= "line" then return end

    if step_mode == "over" and current_func then
        local info = debug.getinfo(2, "f")
        if info.func ~= current_func then return end
    end

    local info = debug.getinfo(2, "Snl")
    if not info then return end

    print_break_location(info)
    print_frame_source(info)

    while true do
        io.write(COLOR_RED .. "cdb> " .. COLOR_RESET)
        local input = io.read()
        if not input then return end

        -- Handle empty input - reuse last command
        if input == "" then
            input = last_cmd
        else
            last_cmd = input  -- Update last_cmd only for non-empty input
        end

        local cmd, args = input:match("^(%S+)%s*(.*)")
        cmd = cmd or ""

        if cmd == "c" then
            step_mode = nil
            debug.sethook()
            return
        elseif cmd == "s" then
            step_mode = "into"
            return
        elseif cmd == "n" then
            step_mode = "over"
            current_func = debug.getinfo(2, "f").func
            return
        elseif cmd == "f" then
            step_mode = "out"
            current_func = debug.getinfo(2, "f").func
            return
        elseif cmd == "l" then
            print_locals(2 + current_frame)
        elseif cmd == "t" then
            print_stack_trace()
        elseif cmd == "w" then
            local count = tonumber(args) or 5
            print_frame_source(info, count)
        elseif cmd == "u" then
            local new_frame = current_frame + 1
            local frame_info = debug.getinfo(new_frame + 2, "Snl")
            if frame_info then
                current_frame = new_frame
                print_break_location(frame_info)
                print_frame_source(frame_info)
            else
                print("Already at top of stack")
            end
        elseif cmd == "d" then
            if current_frame > 0 then
                current_frame = current_frame - 1
                local frame_info = debug.getinfo(current_frame + 2, "Snl")
                print_break_location(frame_info)
                print_frame_source(frame_info)
            else
                print("Already at bottom of stack")
            end
        elseif cmd == "p" then
            evaluate_expression(args, 2 + current_frame)
        elseif cmd == "h" then
            print_help()
        elseif cmd == "q" then
            os.exit(0)
        else
            print(COLOR_RED .. "Unknown command. Type 'h' for help." .. COLOR_RESET)
        end
    end
end

-- Make dbg callable
setmetatable(dbg, {
    __call = function(_, condition)
        if condition then return end
        current_frame = 0
        step_mode = "into"
        debug.sethook(debug_hook, "l")
    end
})

-- Expose API
dbg.pretty = pretty
dbg.pretty_depth = 3
dbg.auto_where = false

return dbg
''';

function luaopen_debugger(lua: Plua_State): Integer; cdecl;
begin
  if (luaL_loadbufferx(lua, DEBUGGER_LUA, Length(DEBUGGER_LUA), '<debugger.lua>', nil) <> 0) or
     (lua_pcall(lua, 0, LUA_MULTRET, 0) <> 0) then
    lua_error(lua);
  Result := 1;
end;

const
  MODULE_NAME: PAnsiChar = 'DEBUGGER_LUA_MODULE';
  MSGH: PAnsiChar = 'DEBUGGER_LUA_MSGH';

procedure dbg_setup(lua: Plua_State; name: PAnsiChar; globalName: PAnsiChar; readFunc: lua_CFunction; writeFunc: lua_CFunction); cdecl;
begin
  // Check that the module name was not already defined.
  lua_getfield(lua, LUA_REGISTRYINDEX, MODULE_NAME);
  Assert(lua_isnil(lua, -1) or (System.AnsiStrings.StrComp(name, luaL_checkstring(lua, -1)) = 0));
  lua_pop(lua, 1);

  // Push the module name into the registry.
  lua_pushstring(lua, name);
  lua_setfield(lua, LUA_REGISTRYINDEX, MODULE_NAME);

  // Preload the module
  luaL_requiref(lua, name, luaopen_debugger, 0);

  // Insert the msgh function into the registry.
  lua_getfield(lua, -1, 'msgh');
  lua_setfield(lua, LUA_REGISTRYINDEX, MSGH);

  if Assigned(readFunc) then
  begin
    lua_pushcfunction(lua, readFunc);
    lua_setfield(lua, -2, 'read');
  end;

  if Assigned(writeFunc) then
  begin
    lua_pushcfunction(lua, writeFunc);
    lua_setfield(lua, -2, 'write');
  end;

  if globalName <> nil then
  begin
    lua_setglobal(lua, globalName);
  end else
  begin
    lua_pop(lua, 1);
  end;
end;

procedure dbg_setup_default(lua: Plua_State); cdecl;
begin
  dbg_setup(lua, 'debugger', 'dbg', nil, nil);
end;

function dbg_pcall(lua: Plua_State; nargs: Integer; nresults: Integer; msgh: Integer): Integer; cdecl;
begin
  // Call regular lua_pcall() if a message handler is provided.
  if msgh <> 0 then
    Exit(lua_pcall(lua, nargs, nresults, msgh));

  // Grab the msgh function out of the registry.
  lua_getfield(lua, LUA_REGISTRYINDEX, PUTF8Char(MSGH));
  if lua_isnil(lua, -1) then
    luaL_error(lua, 'Tried to call dbg_call() before calling dbg_setup().');

  // Move the error handler just below the function.
  msgh := lua_gettop(lua) - (1 + nargs);
  lua_insert(lua, msgh);

  // Call the function.
  Result := lua_pcall(lua, nargs, nresults, msgh);

  // Remove the debug handler.
  lua_remove(lua, msgh);
end;

function dbg_dofile(lua: Plua_State; filename: PAnsiChar): Integer;
begin
  Result := luaL_loadfile(lua, filename);
  if Result = 0 then
    Result := dbg_pcall(lua, 0, LUA_MULTRET, 0);
end;
{$ENDREGION}

{$ENDREGION}

{$REGION ' CHANDRA '}

{$REGION ' LUA CODE '}
const cLOADER_LUA : array[1..436] of Byte = (
$2D, $2D, $20, $55, $74, $69, $6C, $69, $74, $79, $20, $66, $75, $6E, $63, $74,
$69, $6F, $6E, $20, $66, $6F, $72, $20, $68, $61, $76, $69, $6E, $67, $20, $61,
$20, $77, $6F, $72, $6B, $69, $6E, $67, $20, $69, $6D, $70, $6F, $72, $74, $20,
$66, $75, $6E, $63, $74, $69, $6F, $6E, $0A, $2D, $2D, $20, $46, $65, $65, $6C,
$20, $66, $72, $65, $65, $20, $74, $6F, $20, $75, $73, $65, $20, $69, $74, $20,
$69, $6E, $20, $79, $6F, $75, $72, $20, $6F, $77, $6E, $20, $70, $72, $6F, $6A,
$65, $63, $74, $73, $0A, $28, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $29,
$0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20, $73, $63, $72, $69, $70,
$74, $5F, $63, $61, $63, $68, $65, $20, $3D, $20, $7B, $7D, $3B, $0A, $20, $20,
$20, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $69, $6D, $70, $6F, $72,
$74, $28, $6E, $61, $6D, $65, $29, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
$69, $66, $20, $73, $63, $72, $69, $70, $74, $5F, $63, $61, $63, $68, $65, $5B,
$6E, $61, $6D, $65, $5D, $20, $3D, $3D, $20, $6E, $69, $6C, $20, $74, $68, $65,
$6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $73, $63,
$72, $69, $70, $74, $5F, $63, $61, $63, $68, $65, $5B, $6E, $61, $6D, $65, $5D,
$20, $3D, $20, $6C, $6F, $61, $64, $66, $69, $6C, $65, $28, $6E, $61, $6D, $65,
$29, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $65, $6E, $64, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $69,
$66, $20, $73, $63, $72, $69, $70, $74, $5F, $63, $61, $63, $68, $65, $5B, $6E,
$61, $6D, $65, $5D, $20, $7E, $3D, $20, $6E, $69, $6C, $20, $74, $68, $65, $6E,
$0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $72, $65, $74,
$75, $72, $6E, $20, $73, $63, $72, $69, $70, $74, $5F, $63, $61, $63, $68, $65,
$5B, $6E, $61, $6D, $65, $5D, $28, $29, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $65, $6E, $64, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $65, $72, $72,
$6F, $72, $28, $22, $46, $61, $69, $6C, $65, $64, $20, $74, $6F, $20, $6C, $6F,
$61, $64, $20, $73, $63, $72, $69, $70, $74, $20, $22, $20, $2E, $2E, $20, $6E,
$61, $6D, $65, $29, $0A, $20, $20, $20, $20, $65, $6E, $64, $0A, $65, $6E, $64,
$29, $28, $29, $0A
);

const cLUABUNDLE_LUA : array[1..3478] of Byte = (
$28, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $61, $72, $67, $73, $29, $0D,
$0A, $6C, $6F, $63, $61, $6C, $20, $6D, $6F, $64, $75, $6C, $65, $73, $20, $3D,
$20, $7B, $7D, $0D, $0A, $6D, $6F, $64, $75, $6C, $65, $73, $5B, $27, $61, $70,
$70, $2F, $62, $75, $6E, $64, $6C, $65, $5F, $6D, $61, $6E, $61, $67, $65, $72,
$2E, $6C, $75, $61, $27, $5D, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F,
$6E, $28, $2E, $2E, $2E, $29, $0D, $0A, $2D, $2D, $20, $43, $6C, $61, $73, $73,
$20, $66, $6F, $72, $20, $63, $6F, $6C, $6C, $65, $63, $74, $69, $6E, $67, $20,
$74, $68, $65, $20, $66, $69, $6C, $65, $27, $73, $20, $63, $6F, $6E, $74, $65,
$6E, $74, $20, $61, $6E, $64, $20, $62, $75, $69, $6C, $64, $69, $6E, $67, $20,
$61, $20, $62, $75, $6E, $64, $6C, $65, $20, $66, $69, $6C, $65, $0D, $0A, $6C,
$6F, $63, $61, $6C, $20, $73, $6F, $75, $72, $63, $65, $5F, $70, $61, $72, $73,
$65, $72, $20, $3D, $20, $69, $6D, $70, $6F, $72, $74, $28, $22, $61, $70, $70,
$2F, $73, $6F, $75, $72, $63, $65, $5F, $70, $61, $72, $73, $65, $72, $2E, $6C,
$75, $61, $22, $29, $0D, $0A, $0D, $0A, $72, $65, $74, $75, $72, $6E, $20, $66,
$75, $6E, $63, $74, $69, $6F, $6E, $28, $65, $6E, $74, $72, $79, $5F, $70, $6F,
$69, $6E, $74, $29, $0D, $0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20,
$73, $65, $6C, $66, $20, $3D, $20, $7B, $7D, $0D, $0A, $20, $20, $20, $20, $6C,
$6F, $63, $61, $6C, $20, $66, $69, $6C, $65, $73, $20, $3D, $20, $7B, $7D, $0D,
$0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $2D, $2D, $20, $53, $65,
$61, $72, $63, $68, $65, $73, $20, $74, $68, $65, $20, $67, $69, $76, $65, $6E,
$20, $66, $69, $6C, $65, $20, $72, $65, $63, $75, $72, $73, $69, $76, $65, $6C,
$79, $20, $66, $6F, $72, $20, $69, $6D, $70, $6F, $72, $74, $20, $66, $75, $6E,
$63, $74, $69, $6F, $6E, $20, $63, $61, $6C, $6C, $73, $0D, $0A, $20, $20, $20,
$20, $73, $65, $6C, $66, $2E, $70, $72, $6F, $63, $65, $73, $73, $5F, $66, $69,
$6C, $65, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $66, $69,
$6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $6C, $6F, $63, $61, $6C, $20, $70, $61, $72, $73, $65, $72, $20, $3D, $20,
$73, $6F, $75, $72, $63, $65, $5F, $70, $61, $72, $73, $65, $72, $28, $66, $69,
$6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $66, $69, $6C, $65, $73, $5B, $66, $69, $6C, $65, $6E, $61, $6D, $65, $5D,
$20, $3D, $20, $70, $61, $72, $73, $65, $72, $2E, $63, $6F, $6E, $74, $65, $6E,
$74, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $0D, $0A, $20, $20, $20,
$20, $20, $20, $20, $20, $66, $6F, $72, $20, $5F, $2C, $20, $66, $20, $69, $6E,
$20, $70, $61, $69, $72, $73, $28, $70, $61, $72, $73, $65, $72, $2E, $69, $6E,
$63, $6C, $75, $64, $65, $73, $29, $20, $64, $6F, $0D, $0A, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $20, $73, $65, $6C, $66, $2E, $70, $72, $6F,
$63, $65, $73, $73, $5F, $66, $69, $6C, $65, $28, $66, $29, $0D, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $65,
$6E, $64, $0D, $0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $2D, $2D,
$20, $43, $72, $65, $61, $74, $65, $20, $61, $20, $62, $75, $6E, $64, $6C, $65,
$20, $66, $69, $6C, $65, $20, $77, $68, $69, $63, $68, $20, $63, $6F, $6E, $74,
$61, $69, $6E, $73, $20, $74, $68, $65, $20, $64, $65, $74, $65, $63, $74, $65,
$64, $20, $66, $69, $6C, $65, $73, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C,
$66, $2E, $62, $75, $69, $6C, $64, $5F, $62, $75, $6E, $64, $6C, $65, $20, $3D,
$20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $64, $65, $73, $74, $5F, $66,
$69, $6C, $65, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $6C, $6F,
$63, $61, $6C, $20, $66, $69, $6C, $65, $20, $3D, $20, $69, $6F, $2E, $6F, $70,
$65, $6E, $28, $64, $65, $73, $74, $5F, $66, $69, $6C, $65, $2C, $20, $22, $77,
$22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $0D, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65,
$28, $22, $28, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $61, $72, $67, $73,
$29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66,
$69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $6C, $6F, $63, $61, $6C,
$20, $6D, $6F, $64, $75, $6C, $65, $73, $20, $3D, $20, $7B, $7D, $5C, $6E, $22,
$29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $0D, $0A, $20, $20, $20,
$20, $20, $20, $20, $20, $2D, $2D, $20, $43, $72, $65, $61, $74, $65, $20, $61,
$20, $73, $6F, $72, $74, $65, $64, $20, $6C, $69, $73, $74, $20, $6F, $66, $20,
$6B, $65, $79, $73, $20, $73, $6F, $20, $74, $68, $65, $20, $6F, $75, $74, $70,
$75, $74, $20, $77, $69, $6C, $6C, $20, $62, $65, $20, $74, $68, $65, $20, $73,
$61, $6D, $65, $20, $77, $68, $65, $6E, $20, $74, $68, $65, $20, $69, $6E, $70,
$75, $74, $20, $64, $6F, $65, $73, $20, $6E, $6F, $74, $20, $63, $68, $61, $6E,
$67, $65, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $6C, $6F, $63, $61,
$6C, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $73, $20, $3D, $20, $7B, $7D,
$0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $6F, $72, $20, $66, $69,
$6C, $65, $6E, $61, $6D, $65, $2C, $20, $5F, $20, $69, $6E, $20, $70, $61, $69,
$72, $73, $28, $66, $69, $6C, $65, $73, $29, $20, $64, $6F, $0D, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $74, $61, $62, $6C, $65, $2E,
$69, $6E, $73, $65, $72, $74, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $73,
$2C, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20,
$20, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $20, $20,
$20, $20, $74, $61, $62, $6C, $65, $2E, $73, $6F, $72, $74, $28, $66, $69, $6C,
$65, $6E, $61, $6D, $65, $73, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $2D, $2D, $20, $41, $64,
$64, $20, $66, $69, $6C, $65, $73, $20, $61, $73, $20, $6D, $6F, $64, $75, $6C,
$65, $73, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $6F, $72, $20,
$5F, $2C, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $20, $69, $6E, $20, $70,
$61, $69, $72, $73, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $73, $29, $20,
$64, $6F, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
$66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $6D, $6F, $64, $75,
$6C, $65, $73, $5B, $27, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28,
$66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74,
$65, $28, $22, $27, $5D, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E,
$28, $2E, $2E, $2E, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74,
$65, $28, $66, $69, $6C, $65, $73, $5B, $66, $69, $6C, $65, $6E, $61, $6D, $65,
$5D, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
$66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $5C, $6E, $22, $29,
$0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69,
$6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $65, $6E, $64, $5C, $6E, $22,
$29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A,
$20, $20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69,
$74, $65, $28, $22, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $69, $6D, $70,
$6F, $72, $74, $28, $6E, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20,
$20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22,
$72, $65, $74, $75, $72, $6E, $20, $6D, $6F, $64, $75, $6C, $65, $73, $5B, $6E,
$5D, $28, $74, $61, $62, $6C, $65, $2E, $75, $6E, $70, $61, $63, $6B, $28, $61,
$72, $67, $73, $29, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20,
$20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $65,
$6E, $64, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
$0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77,
$72, $69, $74, $65, $28, $22, $6C, $6F, $63, $61, $6C, $20, $65, $6E, $74, $72,
$79, $20, $3D, $20, $69, $6D, $70, $6F, $72, $74, $28, $27, $22, $20, $2E, $2E,
$20, $65, $6E, $74, $72, $79, $5F, $70, $6F, $69, $6E, $74, $20, $2E, $2E, $20,
$22, $27, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A,
$77, $72, $69, $74, $65, $28, $22, $65, $6E, $64, $29, $28, $7B, $2E, $2E, $2E,
$7D, $29, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69,
$6C, $65, $3A, $66, $6C, $75, $73, $68, $28, $29, $0D, $0A, $20, $20, $20, $20,
$20, $20, $20, $20, $66, $69, $6C, $65, $3A, $63, $6C, $6F, $73, $65, $28, $29,
$0D, $0A, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $0D,
$0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $20, $73, $65, $6C, $66,
$0D, $0A, $65, $6E, $64, $0D, $0A, $65, $6E, $64, $0D, $0A, $6D, $6F, $64, $75,
$6C, $65, $73, $5B, $27, $61, $70, $70, $2F, $6D, $61, $69, $6E, $2E, $6C, $75,
$61, $27, $5D, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $2E,
$2E, $2E, $29, $0D, $0A, $2D, $2D, $20, $4D, $61, $69, $6E, $20, $66, $75, $6E,
$63, $74, $69, $6F, $6E, $20, $6F, $66, $20, $74, $68, $65, $20, $70, $72, $6F,
$67, $72, $61, $6D, $0D, $0A, $6C, $6F, $63, $61, $6C, $20, $62, $75, $6E, $64,
$6C, $65, $5F, $6D, $61, $6E, $61, $67, $65, $72, $20, $3D, $20, $69, $6D, $70,
$6F, $72, $74, $28, $22, $61, $70, $70, $2F, $62, $75, $6E, $64, $6C, $65, $5F,
$6D, $61, $6E, $61, $67, $65, $72, $2E, $6C, $75, $61, $22, $29, $0D, $0A, $0D,
$0A, $72, $65, $74, $75, $72, $6E, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E,
$28, $61, $72, $67, $73, $29, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $23,
$61, $72, $67, $73, $20, $3D, $3D, $20, $31, $20, $61, $6E, $64, $20, $61, $72,
$67, $73, $5B, $31, $5D, $20, $3D, $3D, $20, $22, $2D, $76, $22, $20, $74, $68,
$65, $6E, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $70, $72, $69, $6E,
$74, $28, $22, $6C, $75, $61, $62, $75, $6E, $64, $6C, $65, $20, $76, $30, $2E,
$30, $31, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $6F, $73,
$2E, $65, $78, $69, $74, $28, $29, $0D, $0A, $20, $20, $20, $20, $65, $6C, $73,
$65, $69, $66, $20, $23, $61, $72, $67, $73, $20, $7E, $3D, $20, $32, $20, $74,
$68, $65, $6E, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $70, $72, $69,
$6E, $74, $28, $22, $75, $73, $61, $67, $65, $3A, $20, $6C, $75, $61, $62, $75,
$6E, $64, $6C, $65, $20, $69, $6E, $20, $6F, $75, $74, $22, $29, $0D, $0A, $20,
$20, $20, $20, $20, $20, $20, $20, $6F, $73, $2E, $65, $78, $69, $74, $28, $29,
$0D, $0A, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $0D,
$0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20, $69, $6E, $66, $69, $6C,
$65, $20, $3D, $20, $61, $72, $67, $73, $5B, $31, $5D, $0D, $0A, $20, $20, $20,
$20, $6C, $6F, $63, $61, $6C, $20, $6F, $75, $74, $66, $69, $6C, $65, $20, $3D,
$20, $61, $72, $67, $73, $5B, $32, $5D, $0D, $0A, $20, $20, $20, $20, $6C, $6F,
$63, $61, $6C, $20, $62, $75, $6E, $64, $6C, $65, $20, $3D, $20, $62, $75, $6E,
$64, $6C, $65, $5F, $6D, $61, $6E, $61, $67, $65, $72, $28, $69, $6E, $66, $69,
$6C, $65, $29, $0D, $0A, $20, $20, $20, $20, $62, $75, $6E, $64, $6C, $65, $2E,
$70, $72, $6F, $63, $65, $73, $73, $5F, $66, $69, $6C, $65, $28, $69, $6E, $66,
$69, $6C, $65, $2C, $20, $62, $75, $6E, $64, $6C, $65, $29, $0D, $0A, $20, $20,
$20, $20, $0D, $0A, $20, $20, $20, $20, $62, $75, $6E, $64, $6C, $65, $2E, $62,
$75, $69, $6C, $64, $5F, $62, $75, $6E, $64, $6C, $65, $28, $6F, $75, $74, $66,
$69, $6C, $65, $29, $0D, $0A, $65, $6E, $64, $0D, $0A, $65, $6E, $64, $0D, $0A,
$6D, $6F, $64, $75, $6C, $65, $73, $5B, $27, $61, $70, $70, $2F, $73, $6F, $75,
$72, $63, $65, $5F, $70, $61, $72, $73, $65, $72, $2E, $6C, $75, $61, $27, $5D,
$20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $2E, $2E, $2E, $29,
$0D, $0A, $2D, $2D, $20, $43, $6C, $61, $73, $73, $20, $66, $6F, $72, $20, $65,
$78, $74, $72, $61, $63, $74, $69, $6E, $67, $20, $69, $6D, $70, $6F, $72, $74,
$20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $63, $61, $6C, $6C, $73, $20,
$66, $72, $6F, $6D, $20, $73, $6F, $75, $72, $63, $65, $20, $66, $69, $6C, $65,
$73, $0D, $0A, $72, $65, $74, $75, $72, $6E, $20, $66, $75, $6E, $63, $74, $69,
$6F, $6E, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20,
$20, $20, $6C, $6F, $63, $61, $6C, $20, $66, $69, $6C, $65, $20, $3D, $20, $69,
$6F, $2E, $6F, $70, $65, $6E, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $2C,
$20, $22, $72, $22, $29, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $66, $69,
$6C, $65, $20, $3D, $3D, $20, $6E, $69, $6C, $20, $74, $68, $65, $6E, $0D, $0A,
$20, $20, $20, $20, $20, $20, $20, $20, $65, $72, $72, $6F, $72, $28, $22, $46,
$69, $6C, $65, $20, $6E, $6F, $74, $20, $66, $6F, $75, $6E, $64, $3A, $20, $22,
$20, $2E, $2E, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20,
$20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $6C, $6F, $63, $61,
$6C, $20, $66, $69, $6C, $65, $5F, $63, $6F, $6E, $74, $65, $6E, $74, $20, $3D,
$20, $66, $69, $6C, $65, $3A, $72, $65, $61, $64, $28, $22, $2A, $61, $22, $29,
$0D, $0A, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $63, $6C, $6F, $73, $65,
$28, $29, $0D, $0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20, $69, $6E,
$63, $6C, $75, $64, $65, $64, $5F, $66, $69, $6C, $65, $73, $20, $3D, $20, $7B,
$7D, $0D, $0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $2D, $2D, $20,
$53, $65, $61, $72, $63, $68, $20, $66, $6F, $72, $20, $69, $6D, $70, $6F, $72,
$74, $28, $29, $20, $63, $61, $6C, $6C, $73, $20, $77, $69, $74, $68, $20, $64,
$6F, $62, $75, $6C, $65, $20, $71, $75, $6F, $74, $65, $73, $20, $28, $21, $29,
$0D, $0A, $20, $20, $20, $20, $66, $6F, $72, $20, $66, $20, $69, $6E, $20, $73,
$74, $72, $69, $6E, $67, $2E, $67, $6D, $61, $74, $63, $68, $28, $66, $69, $6C,
$65, $5F, $63, $6F, $6E, $74, $65, $6E, $74, $2C, $20, $27, $69, $6D, $70, $6F,
$72, $74, $25, $28, $5B, $22, $5C, $27, $5D, $28, $5B, $5E, $5C, $27, $22, $5D,
$2D, $29, $5B, $22, $5C, $27, $5D, $25, $29, $27, $29, $20, $64, $6F, $0D, $0A,
$20, $20, $20, $20, $20, $20, $20, $20, $74, $61, $62, $6C, $65, $2E, $69, $6E,
$73, $65, $72, $74, $28, $69, $6E, $63, $6C, $75, $64, $65, $64, $5F, $66, $69,
$6C, $65, $73, $2C, $20, $66, $29, $0D, $0A, $20, $20, $20, $20, $65, $6E, $64,
$0D, $0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66,
$20, $3D, $20, $7B, $7D, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66, $2E,
$66, $69, $6C, $65, $6E, $61, $6D, $65, $20, $3D, $20, $66, $69, $6C, $65, $6E,
$61, $6D, $65, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66, $2E, $63, $6F,
$6E, $74, $65, $6E, $74, $20, $3D, $20, $66, $69, $6C, $65, $5F, $63, $6F, $6E,
$74, $65, $6E, $74, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66, $2E, $69,
$6E, $63, $6C, $75, $64, $65, $73, $20, $3D, $20, $69, $6E, $63, $6C, $75, $64,
$65, $64, $5F, $66, $69, $6C, $65, $73, $0D, $0A, $20, $20, $20, $20, $72, $65,
$74, $75, $72, $6E, $20, $73, $65, $6C, $66, $0D, $0A, $65, $6E, $64, $0D, $0A,
$65, $6E, $64, $0D, $0A, $6D, $6F, $64, $75, $6C, $65, $73, $5B, $27, $6C, $75,
$61, $62, $75, $6E, $64, $6C, $65, $2E, $6C, $75, $61, $27, $5D, $20, $3D, $20,
$66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $2E, $2E, $2E, $29, $0D, $0A, $2D,
$2D, $20, $45, $6E, $74, $72, $79, $20, $70, $6F, $69, $6E, $74, $20, $6F, $66,
$20, $74, $68, $65, $20, $70, $72, $6F, $67, $72, $61, $6D, $2E, $0D, $0A, $2D,
$2D, $20, $4F, $6E, $6C, $79, $20, $62, $61, $73, $69, $63, $20, $73, $74, $75,
$66, $66, $20, $69, $73, $20, $73, $65, $74, $20, $75, $70, $20, $68, $65, $72,
$65, $2C, $20, $74, $68, $65, $20, $61, $63, $74, $75, $61, $6C, $20, $70, $72,
$6F, $67, $72, $61, $6D, $20, $69, $73, $20, $69, $6E, $20, $61, $70, $70, $2F,
$6D, $61, $69, $6E, $2E, $6C, $75, $61, $0D, $0A, $6C, $6F, $63, $61, $6C, $20,
$61, $72, $67, $73, $20, $3D, $20, $7B, $2E, $2E, $2E, $7D, $0D, $0A, $0D, $0A,
$2D, $2D, $20, $43, $68, $65, $63, $6B, $20, $69, $66, $20, $77, $65, $20, $61,
$72, $65, $20, $61, $6C, $72, $65, $61, $64, $79, $20, $62, $75, $6E, $64, $6C,
$65, $64, $0D, $0A, $69, $66, $20, $69, $6D, $70, $6F, $72, $74, $20, $3D, $3D,
$20, $6E, $69, $6C, $20, $74, $68, $65, $6E, $0D, $0A, $20, $20, $20, $20, $64,
$6F, $66, $69, $6C, $65, $28, $22, $75, $74, $69, $6C, $2F, $6C, $6F, $61, $64,
$65, $72, $2E, $6C, $75, $61, $22, $29, $0D, $0A, $65, $6E, $64, $0D, $0A, $0D,
$0A, $69, $6D, $70, $6F, $72, $74, $28, $22, $61, $70, $70, $2F, $6D, $61, $69,
$6E, $2E, $6C, $75, $61, $22, $29, $28, $61, $72, $67, $73, $29, $0D, $0A, $65,
$6E, $64, $0D, $0A, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $69, $6D, $70,
$6F, $72, $74, $28, $6E, $29, $0D, $0A, $72, $65, $74, $75, $72, $6E, $20, $6D,
$6F, $64, $75, $6C, $65, $73, $5B, $6E, $5D, $28, $74, $61, $62, $6C, $65, $2E,
$75, $6E, $70, $61, $63, $6B, $28, $61, $72, $67, $73, $29, $29, $0D, $0A, $65,
$6E, $64, $0D, $0A, $6C, $6F, $63, $61, $6C, $20, $65, $6E, $74, $72, $79, $20,
$3D, $20, $69, $6D, $70, $6F, $72, $74, $28, $27, $6C, $75, $61, $62, $75, $6E,
$64, $6C, $65, $2E, $6C, $75, $61, $27, $29, $0D, $0A, $65, $6E, $64, $29, $28,
$7B, $2E, $2E, $2E, $7D, $29
);
{$ENDREGION}

type
  { TLuaBridgeRegistry }
  TLuaBridgeRegistry = class
  private
    class var
      FWrapperMap: TDictionary<Pointer, TObject>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterWrapper(Key: Pointer; Wrapper: TObject);
    class function GetWrapper(Key: Pointer): TObject;
    class procedure RemoveWrapper(Key: Pointer);
  end;

{ Lua C functions }
function LuaMethodCallback(L: Plua_State): Integer; cdecl;
var
  LWrapper: TChandraMethodWrapper;
  LWrapperObj: TObject;
begin
  LWrapperObj := TLuaBridgeRegistry.GetWrapper(lua_touserdata(L, lua_upvalueindex(1)));
  if LWrapperObj is TChandraMethodWrapper then
  begin
    LWrapper := TChandraMethodWrapper(LWrapperObj);
    Result := LWrapper.Execute(L);
  end
  else
  begin
    lua_pushstring(L, 'Invalid method wrapper');
    lua_error(L);
    Result := 0;
  end;
end;

function LuaRecordGC(L: Plua_State): Integer; cdecl;
begin
  // Records are value types, so we don't need to do anything special
  // The memory allocated by lua_newuserdata will be freed by Lua
  Result := 0;
end;

// Add these GC functions at unit level
function LuaPointerGC(L: Plua_State): Integer; cdecl;
begin
  // By default, do nothing - pointer cleanup should be handled by the application
  Result := 0;
end;

function LuaObjectGC(L: Plua_State): Integer; cdecl;
var
  LUserData: Pointer;
begin
  LUserData := lua_touserdata(L, 1);
  if LUserData <> nil then
  begin
    TObject(LUserData^).Free;
    TObject(LUserData^) := nil;
  end;
  Result := 0;
end;

{ TLuaBridgeRegistry }
class constructor TLuaBridgeRegistry.Create;
begin
  FWrapperMap := TDictionary<Pointer, TObject>.Create;
end;

class destructor TLuaBridgeRegistry.Destroy;
begin
  FWrapperMap.Free;
end;

class procedure TLuaBridgeRegistry.RegisterWrapper(Key: Pointer; Wrapper: TObject);
begin
  FWrapperMap.AddOrSetValue(Key, Wrapper);
end;

class function TLuaBridgeRegistry.GetWrapper(Key: Pointer): TObject;
begin
  if not FWrapperMap.TryGetValue(Key, Result) then
    Result := nil;
end;

class procedure TLuaBridgeRegistry.RemoveWrapper(Key: Pointer);
begin
  FWrapperMap.Remove(Key);
end;

{ TChandraMethodWrapper }
constructor TChandraMethodWrapper.Create(const AMethod: TRttiMethod; const AClass: TClass);
begin
  inherited Create();

  FMethod := AMethod;
  FClass := AClass;
  FContext := TRttiContext.Create;
end;

function TChandraMethodWrapper.ConvertNativeToLua(const AState: Pointer; const AValue: TValue): Integer;
var
  LObj: TObject;
  LUserData: Pointer;
  LRecordSize: Integer;
begin
  case AValue.Kind of
    tkInteger:
      begin
        lua_pushinteger(AState, AValue.AsInteger);
        Result := 1;
      end;
    tkFloat:
      begin
        lua_pushnumber(AState, AValue.AsExtended);
        Result := 1;
      end;
    tkString, tkUString:
      begin
        lua_pushstring(AState, AsUTF8(AValue.AsString));
        Result := 1;
      end;
    tkEnumeration:
      begin
        if AValue.TypeInfo = TypeInfo(Boolean) then
          lua_pushboolean(AState, Ord(AValue.AsBoolean))
        else
          lua_pushinteger(AState, AValue.AsOrdinal);
        Result := 1;
      end;
    tkClass:
      begin
        if AValue.IsObject then
        begin
          LObj := AValue.AsObject;
          if LObj <> nil then
          begin
            LUserData := lua_newuserdata(AState, SizeOf(TObject));
            TObject(LUserData^) := LObj;

            if luaL_newmetatable(AState, AsUTF8(LObj.ClassName)) <> 0 then
            begin
              lua_pushstring(AState, '__gc');
              lua_pushcclosure(AState, @LuaObjectGC, 0);
              lua_rawset(AState, -3);
            end;
            lua_setmetatable(AState, -2);
          end
          else
            lua_pushnil(AState);
        end
        else
          lua_pushnil(AState);
        Result := 1;
      end;
    tkPointer:
      begin
        if AValue.IsEmpty then
          lua_pushnil(AState)
        else
        begin
          LUserData := lua_newuserdata(AState, SizeOf(Pointer));
          PPointer(LUserData)^ := AValue.AsType<Pointer>;
        end;
        Result := 1;
      end;
    tkRecord:
      begin
        // Get the size of the record from RTTI
        LRecordSize := AValue.DataSize;
        // Allocate userdata to hold the record
        LUserData := lua_newuserdata(AState, LRecordSize);
        // Copy the record data
        Move(AValue.GetReferenceToRawData^, LUserData^, LRecordSize);
        // Add a metatable with the record type name
        if luaL_newmetatable(AState, AsUTF8(string(AValue.TypeInfo.Name))) <> 0 then
        //if luaL_newmetatable(AState, PAnsiChar(AnsiString(AValue.TypeInfo.Name))) <> 0 then
        begin
          // Set up metatable if needed
          lua_pushstring(AState, '__gc');
          lua_pushcclosure(AState, @LuaRecordGC, 0);
          lua_rawset(AState, -3);
        end;
        lua_setmetatable(AState, -2);
        Result := 1;
      end;
    else
      begin
        lua_pushnil(AState);
        Result := 1;
      end;
  end;
end;

function TChandraMethodWrapper.ConvertLuaToNative(const AState: Pointer; const AParamType: TRttiType; const AStackIndex: Integer): TValue;
var
  LUserData: Pointer;
  LTypeInfo: PTypeInfo;
  LRecordValue: TValue;
  LRecordSize: Integer;
begin
  case AParamType.TypeKind of
    tkInteger:
      Result := TValue.From<Integer>(lua_tointeger(AState, AStackIndex));
    tkFloat:
      Result := TValue.From<Double>(lua_tonumber(AState, AStackIndex));
    tkString, tkUString:
      Result := TValue.From<string>(string(lua_tostring(AState, AStackIndex)));
    tkEnumeration:
      if AParamType.Handle = TypeInfo(Boolean) then
        Result := TValue.From<Boolean>(LongBool(lua_toboolean(AState, AStackIndex)))
      else
        Result := TValue.FromOrdinal(AParamType.Handle, lua_tointeger(AState, AStackIndex));
    tkClass:
      begin
        if lua_type(AState, AStackIndex) = LUA_TUSERDATA then
        begin
          LUserData := lua_touserdata(AState, AStackIndex);
          if LUserData <> nil then
            Result := TValue.From<TObject>(TObject(LUserData^))
          else
            Result := TValue.From<TObject>(nil);
        end
        else
          Result := TValue.From<TObject>(nil);
      end;

    tkPointer:
      begin
        case lua_type(AState, AStackIndex) of
          LUA_TUSERDATA:
            begin
              LUserData := lua_touserdata(AState, AStackIndex);
              if LUserData <> nil then
              begin
                // First check if it's a pointer to a pointer (regular pointer case)
                if luaL_getmetatable(AState, AsUTF8(AParamType.Name)) = 1 then
                begin
                  lua_pop(AState, 1);  // Pop metatable
                  Result := TValue.From<Pointer>(LUserData);
                end
                else
                begin
                  // Check if we have any metatable (could be a record)
                  if lua_getmetatable(AState, AStackIndex) = 1 then
                  begin
                    lua_pop(AState, 1); // Pop metatable
                    // If it has a metatable, treat it as a record pointer
                    Result := TValue.From<Pointer>(LUserData);
                  end
                  else
                    // No metatable, treat as regular pointer
                    Result := TValue.From<Pointer>(PPointer(LUserData)^);
                end;
              end
              else
                Result := TValue.From<Pointer>(nil);
            end;
          LUA_TLIGHTUSERDATA:
            Result := TValue.From<Pointer>(lua_touserdata(AState, AStackIndex));
        else
          Result := TValue.From<Pointer>(nil);
        end;
      end;

    tkRecord:
      begin
        if lua_type(AState, AStackIndex) = LUA_TUSERDATA then
        begin
          LUserData := lua_touserdata(AState, AStackIndex);
          if LUserData <> nil then
          begin
            LTypeInfo := AParamType.Handle;
            LRecordSize := AParamType.TypeSize;

            // Create a new TValue to hold the record
            TValue.Make(nil, LTypeInfo, LRecordValue);
            // Copy the data from Lua userdata to the new record
            Move(LUserData^, LRecordValue.GetReferenceToRawData^, LRecordSize);
            Result := LRecordValue;
          end
          else
            raise EChandraRegistrationError.Create('Nil userdata for record parameter');
        end
        else
          raise EChandraRegistrationError.Create('Expected userdata for record parameter');
      end;
    else
      raise EChandraRegistrationError.CreateFmt('Unsupported parameter type: %s',
        [AParamType.Name]);
  end;
end;

function TChandraMethodWrapper.Execute(const AState: Pointer): Integer;
var
  LParams: array of TValue;
  LParam: TRttiParameter;
  LReturnValue: TValue;
  I: Integer;
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := FMethod.GetParameters;
  SetLength(LParams, Length(LParameters));

  // Convert Lua parameters to native types
  for I := 0 to High(LParameters) do
  begin
    LParam := LParameters[I];
    LParams[I] := ConvertLuaToNative(AState, LParam.ParamType, I + 1);
  end;

  try
    // Execute the class method
    LReturnValue := FMethod.Invoke(FClass, LParams);

    // Convert return value to Lua
    if FMethod.ReturnType <> nil then
      Result := ConvertNativeToLua(AState, LReturnValue)
    else
      Result := 0;
  except
    on E: Exception do
    begin
      lua_pushstring(AState, AsUTF8(E.Message));
      lua_error(AState);
      Result := 0;
    end;
  end;
end;

{ TChandra }
constructor TChandra.Create();
begin
  inherited;

  Open();
end;

destructor TChandra.Destroy;
begin
  Close();

  inherited;
end;

function TChandra.Open(): Boolean;
begin
  Result := False;

  FContext := TRttiContext.Create;
  FWrappers := TObjectDictionary<string, TChandraMethodWrapper>.Create([doOwnsValues]);

  FState := luaL_newstate();
  if not Assigned(FState) then Exit;

   SetGCStepSize(200);

  luaL_openlibs(FState);

  LoadBuffer(@cLOADER_LUA, Length(cLOADER_LUA));

  // Create the Chandra table
  lua_newtable(FState); // Pushes a new table onto the stack (at index -1)

  // Create global Chandra table
  SetVariable('Chandra.luaVersion', Format('%d.%d.%d', [LUA_VERSION_MAJOR_N, LUA_VERSION_MINOR_N, LUA_VERSION_RELEASE_N]));
  SetVariable('Chandra.version', CHANDRA_VERSION_FULL);

  dbg_setup_default(FState);

  Result := True;
end;

procedure TChandra.Close();
var
  LWrapper: TChandraMethodWrapper;
begin
  if not Assigned(FState) then Exit;

  lua_close(FState);
  FState := nil;

  for LWrapper in FWrappers.Values do
    TLuaBridgeRegistry.RemoveWrapper(Pointer(LWrapper));

  FWrappers.Free;
  FContext.Free;
end;

procedure TChandra.Reset();
begin
  OnBeforeReset();
  Close();
  Open();
  OnAfterReset();
end;

procedure TChandra.ValidateMethod(const AMethod: TRttiMethod);
var
  LParam: TRttiParameter;
  LValidType: Boolean;
begin
  if not AMethod.IsClassMethod then
    raise EChandraRegistrationError.CreateFmt(
      'Method %s must be a class method (declare with class keyword)',
      [AMethod.Name]);

  if AMethod.ReturnType <> nil then
  begin
    LValidType := AMethod.ReturnType.TypeKind in
      [tkInteger, tkFloat, tkString, tkUString, tkEnumeration, tkClass, tkPointer, tkRecord];
    if not LValidType then
      raise EChandraRegistrationError.CreateFmt(
        'Unsupported return type for method %s: %s',
        [AMethod.Name, AMethod.ReturnType.Name]);
  end;

  for LParam in AMethod.GetParameters do
  begin
    LValidType := LParam.ParamType.TypeKind in
      [tkInteger, tkFloat, tkString, tkUString, tkEnumeration, tkClass, tkPointer, tkRecord];
    if not LValidType then
      raise EChandraRegistrationError.CreateFmt(
        'Unsupported parameter type in method %s, parameter %s: %s',
        [AMethod.Name, LParam.Name, LParam.ParamType.Name]);
  end;
end;

function TChandra.PushValueToLua(const AValue: TValue): Boolean;
var
  LObj: TObject;
  LPtr: Pointer;
  LUserData: Pointer;
  LTypeInfo: PTypeInfo;
begin
  Result := True;
  LTypeInfo := AValue.TypeInfo;

  case AValue.Kind of
    tkInteger:
      lua_pushinteger(FState, AValue.AsInteger);

    tkFloat:
      lua_pushnumber(FState, AValue.AsExtended);

    tkString, tkUString, tkLString:
      lua_pushstring(FState, AsUTF8(AValue.AsString));

    tkEnumeration:
      if LTypeInfo = TypeInfo(Boolean) then
        lua_pushboolean(FState, Ord(AValue.AsBoolean))
      else
        lua_pushinteger(FState, AValue.AsOrdinal);

    tkClass:
      begin
        LObj := AValue.AsObject;
        if LObj <> nil then
        begin
          LUserData := lua_newuserdata(FState, SizeOf(TObject));
          TObject(LUserData^) := LObj;

          if luaL_newmetatable(FState, AsUTF8(LObj.ClassName)) <> 0 then
          begin
            lua_pushstring(FState, '__gc');
            lua_pushcclosure(FState, @LuaObjectGC, 0);
            lua_rawset(FState, -3);
          end;
          lua_setmetatable(FState, -2);
        end
        else
          lua_pushnil(FState);
      end;

    tkPointer:
      begin
        // Raw pointer
        LPtr := AValue.AsType<Pointer>;
        if LPtr <> nil then
          Result := PushPointer(LPtr)
        else
          lua_pushnil(FState);
      end;

    else
      begin
        lua_pushnil(FState);
        Result := False;
      end;
  end;
end;

function TChandra.GetValueFromLua(const AStackIndex: Integer): TValue;
var
  LUserData: Pointer;
begin
  case lua_type(FState, AStackIndex) of
    LUA_TNIL:
      Result := TValue.Empty;

    LUA_TBOOLEAN:
      Result := TValue.From<Boolean>(LongBool(lua_toboolean(FState, AStackIndex)));

    LUA_TNUMBER:
      begin
        // Check if it's an integer or float
        if LongBool(lua_isinteger(FState, AStackIndex)) then
          Result := TValue.From<Int64>(lua_tointeger(FState, AStackIndex))
        else
          Result := TValue.From<Double>(lua_tonumber(FState, AStackIndex));
      end;

    LUA_TSTRING:
      Result := TValue.From<string>(string(lua_tostring(FState, AStackIndex)));

    LUA_TUSERDATA:
      begin
        LUserData := lua_touserdata(FState, AStackIndex);
        if LUserData <> nil then
        begin
          // Check if it's an object
          if lua_getmetatable(FState, AStackIndex) = 1 then
          begin
            lua_pop(FState, 1); // Pop metatable
            Result := TValue.From<TObject>(TObject(LUserData^));
          end
          else
            // Just a regular pointer
            Result := TValue.From<Pointer>(LUserData);
        end
        else
          Result := TValue.Empty;
      end;

    LUA_TLIGHTUSERDATA:
      Result := TValue.From<Pointer>(lua_touserdata(FState, AStackIndex));

    else
      Result := TValue.Empty;
  end;
end;

function TChandra.LuaParamToString(const AValue: TVarRec): string;
var
  LPointerID: string;
begin
  case AValue.VType of
    vtInteger:
      Result := IntToStr(AValue.VInteger); // Convert integer to string
    vtInt64:
      Result := IntToStr(AValue.VInt64^); // Convert int64 to string
    vtExtended:
      Result := FloatToStr(AValue.VExtended^); // Convert floating-point to string
    vtString:
      Result := '"' + StringReplace(string(AValue.VString^), '"', '\"', [rfReplaceAll]) + '"'; // Escape and quote
    vtAnsiString:
      Result := '"' + StringReplace(string(AnsiString(AValue.VAnsiString)), '"', '\"', [rfReplaceAll]) + '"'; // Escape and quote
    vtUnicodeString:
      Result := '"' + StringReplace(string(UnicodeString(AValue.VUnicodeString)), '"', '\"', [rfReplaceAll]) + '"'; // Escape and quote
    vtBoolean:
      if AValue.VBoolean then
        Result := 'true' // Lua true literal
      else
        Result := 'false'; // Lua false literal
    vtChar:
      Result := '"' + AValue.VChar + '"'; // Single character as string
    vtWideChar:
      Result := '"' + AValue.VWideChar + '"'; // Wide character as string
    vtPointer:
      begin
        if AValue.VPointer = nil then
          Result := 'nil'
        else
        begin
          // Generate a unique ID for the pointer
          LPointerID := Format('_p%p', [AValue.VPointer]);

          // Register the pointer in Lua's global "pointers" table
          lua_getglobal(FState, 'pointers'); // _G["pointers"]
          if lua_type(FState, -1) <> LUA_TTABLE then
          begin
            // Create the "pointers" table if it doesn't exist
            lua_pop(FState, 1);
            lua_newtable(FState);
            lua_setglobal(FState, 'pointers');
            lua_getglobal(FState, 'pointers');
          end;

          // Store the pointer in the "pointers" table
          lua_pushlightuserdata(FState, AValue.VPointer); // Push the pointer as userdata
          lua_setfield(FState, -2, AsUTF8(LPointerID)); // pointers[_p<address>] = userdata
          lua_pop(FState, 1); // Pop the "pointers" table

          // Return the identifier to use in the Lua script
          Result := Format('pointers["%s"]', [LPointerID]);
        end;
      end;
  else
    raise Exception.Create('Unsupported parameter type in LuaParamToString');
  end;
end;

procedure TChandra.RegisterMethod(const AMethod: TRttiMethod; const AClass: TClass);
var
  LWrapper: TChandraMethodWrapper;
  LWrapperPtr: Pointer;
begin
  ValidateMethod(AMethod);

  LWrapper := TChandraMethodWrapper.Create(AMethod, AClass);
  FWrappers.Add(AMethod.Name, LWrapper);

  LWrapperPtr := Pointer(LWrapper);
  TLuaBridgeRegistry.RegisterWrapper(LWrapperPtr, LWrapper);

  lua_pushlightuserdata(FState, LWrapperPtr);
  lua_pushcclosure(FState, @LuaMethodCallback, 1);
  lua_setfield(FState, -2, AsUTF8(AMethod.Name));
end;

function TChandra.PushPointer(const APtr: Pointer; const ATypeInfo: PTypeInfo = nil): Boolean;
var
  LUserData: Pointer;
begin
  Result := True;

  if APtr = nil then
  begin
    lua_pushnil(FState);
    Exit;
  end;

  // Regular pointer
  LUserData := lua_newuserdata(FState, SizeOf(Pointer));
  PPointer(LUserData)^ := APtr;

  // If we know the type, create a metatable for it
  if ATypeInfo <> nil then
  begin
    if luaL_newmetatable(FState, AsUTF8(string(ATypeInfo.Name))) <> 0 then
    begin
      // Could add type-specific metamethods here if needed
      lua_pushstring(FState, '__gc');
      lua_pushcclosure(FState, @LuaPointerGC, 0);
      lua_rawset(FState, -3);
    end;
    lua_setmetatable(FState, -2);
  end;
end;

procedure TChandra.CheckLuaError(const AError: Integer);
var
  LErr: string;
begin
  if FState = nil then Exit;

  case AError of
    // success
    0:
      begin

      end;
    // a runtime error.
    LUA_ERRRUN:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EChandraRuntimeException.CreateFmt('Runtime error [%s]', [LErr]);
      end;
    // memory allocation error. For such errors, Lua does not call the error handler function.
    LUA_ERRMEM:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EChandraException.CreateFmt('Memory allocation error [%s]', [LErr]);
      end;
    // error while running the error handler function.
    LUA_ERRERR:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EChandraException.CreateFmt
          ('Error while running the error handler function [%s]', [LErr]);
      end;
    LUA_ERRSYNTAX:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EChandraSyntaxError.CreateFmt('Syntax Error [%s]', [LErr]);
      end
  else
    begin
      LErr := lua_tostring(FState, -1);
      lua_pop(FState, 1);
      raise EChandraException.CreateFmt('Unknown Error [%s]', [LErr]);
    end;
  end;
end;

function LuaWrapperWriter(aState: Plua_State; const aBuffer: Pointer; aSize: NativeUInt; aData: Pointer): Integer; cdecl;
var
  LStream: TStream;
begin
  LStream := TStream(aData);
  try
    LStream.WriteBuffer(aBuffer^, aSize);
    Result := 0;
  except
    on E: EStreamError do
      Result := 1;
  end;
end;

procedure TChandra.SaveByteCode(const AStream: TStream);
var
  LRet: Integer;
begin
  if not Assigned(FState) then Exit;

  if lua_type(FState, lua_gettop(FState)) <> LUA_TFUNCTION then Exit;

  try
    LRet := lua_dump(FState, LuaWrapperWriter, AStream, 1);
    if LRet <> 0 then
      raise EChandraException.CreateFmt('lua_dump returned code %d', [LRet]);
  finally
    lua_pop(FState, 1);
  end;
end;

procedure TChandra.Bundle(const AInFilename: string; const AOutFilename: string);
var
  LInFilename: string;
  LOutFilename: string;
  LStatus: Integer;
begin
  if FState = nil then Exit;

  if AInFilename.IsEmpty then  Exit;
  if AOutFilename.IsEmpty then Exit;
  LInFilename := AInFilename.Replace('\', '/');
  LOutFilename := AOutFilename.Replace('\', '/');
  LoadBuffer(@cLUABUNDLE_LUA, Length(cLUABUNDLE_LUA), False);

  lua_pushstring(FState, AsUTF8(AInFilename));
  lua_pushstring(FState, AsUTF8(AOutFilename));

  LStatus := lua_pcall(FState, 2, 0, 0);
  CheckLuaError(LStatus);

  lua_pop(FState, lua_gettop(FState));
end;

procedure TChandra.RegisterRoutines(AClass: TClass; const ATableName: string);
var
  LRttiType: TRttiType;
  LMethod: TRttiMethod;
  LActualTableName: string;
begin
  if not Assigned(FState) then Exit;

  LRttiType := FContext.GetType(AClass);

  LActualTableName := ATableName;
  if LActualTableName = '' then
    LActualTableName := AClass.ClassName;

  lua_createtable(FState, 0, 0);

  lua_pushlightuserdata(FState, AClass);
  lua_setfield(FState, -2, '_class');

  for LMethod in LRttiType.GetMethods do
    if (LMethod.Visibility = mvPublished) and LMethod.IsClassMethod then
      RegisterMethod(LMethod, AClass);

  lua_setglobal(FState, AsUTF8(LActualTableName));
end;

(*
function TChandra.LoadFile(const AFilename: string; const AAutoRun: Boolean): Boolean;
var
  LMarshall: TMarshaller;
  LErr: string;
  LRes: Integer;
begin
  Result := False;
  if not Assigned(FState) then Exit;
  if AFilename.IsEmpty then Exit;

  if not TFile.Exists(AFilename) then Exit;
  if AAutoRun then
    LRes := luaL_dofile(FState, LMarshall.AsUtf8(AFilename).ToPointer)
  else
    LRes := luaL_loadfile(FState, LMarshall.AsUtf8(AFilename).ToPointer);
  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EChandraException.Create(LErr);
  end;

  Result := True;
end;
*)

function TChandra.LoadFile(const AFilename: string; const AAutoRun: Boolean): Boolean;
var
  LFileContent: TBytes;
  LErr: string;
  LRes: Integer;
begin
  Result := False;
  if not Assigned(FState) then Exit;
  if AFilename.IsEmpty then Exit;
  if not TFile.Exists(AFilename) then Exit;

  // Read the file content and remove the BOM
  LFileContent := RemoveBOM(TFile.ReadAllBytes(AFilename));

  // Load or execute the script
  LRes := luaL_loadbuffer(FState, AsUTF8(TEncoding.UTF8.GetString(LFileContent)), Length(LFileContent), AsUtf8(AFilename));

  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EChandraException.CreateFmt('%s: %s', [AFilename, LErr]);
  end;

  // Execute the script if AAutoRun is True
  if AAutoRun and (lua_pcall(FState, 0, LUA_MULTRET, 0) <> 0) then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EChandraException.CreateFmt('%s: %s', [AFilename, LErr]);
  end;

  Result := True;
end;

procedure TChandra.LoadString(const AData: string; const AAutoRun: Boolean);
var
  LErr: string;
  LRes: Integer;
  LData: string;
begin
  if not Assigned(FState) then Exit;

  LData := AData;
  if LData.IsEmpty then Exit;

  if AAutoRun then
    LRes := luaL_dostring(FState, AsUTF8(LData, True))
  else
    LRes := luaL_loadstring(FState, AsUTF8(LData, True));

  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EChandraException.Create(LErr);
  end;
end;

procedure TChandra.LoadBuffer(const AData: Pointer; const ASize: NativeUInt; const AAutoRun: Boolean);
var
  LMemStream: TMemoryStream;
  LRes: Integer;
  LErr: string;
  LSize: NativeUInt;
begin
  if not Assigned(FState) then Exit;

  LMemStream := TMemoryStream.Create;
  try
    LMemStream.Write(AData^, ASize);
    LMemStream.Position := 0;
    LSize := LMemStream.Size;
    if AAutoRun then
      LRes := luaL_dobuffer(FState, LMemStream.Memory, LSize, 'LoadBuffer')
    else
      LRes := luaL_loadbuffer(FState, LMemStream.Memory, LSize, 'LoadBuffer');
  finally
    FreeAndNil(LMemStream);
  end;

  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EChandraException.Create(LErr);
  end;
end;

function TChandra.RoutineExist(const AName: string): Boolean;
var
  LStatus: Integer;
begin
  // Attempt to load the name as a Lua expression
  LStatus := luaL_loadstring(FState, AsUTF8('return ' + AName));

  if LStatus = LUA_OK then
  begin
    // Execute the loaded chunk
    if lua_pcall(FState, 0, 1, 0) = LUA_OK then
      Result := lua_isfunction(FState, -1) or LongBool(lua_iscfunction(FState, -1))
    else
      Result := False;
  end
  else
    Result := False;

  // Clean up the Lua stack
  lua_settop(FState, 0);
end;

function TChandra.VariableExist(const AName: string): Boolean;
var
  LTokens: TArray<string>;
  I: Integer;
begin
  Result := False;

  // Split the variable name into tokens
  LTokens := AName.Split(['.']);
  if Length(LTokens) = 0 then
    Exit; // Invalid input

  // Get the base variable
  lua_getglobal(FState, AsUTF8(LTokens[0]));

  for I := 1 to High(LTokens) do
  begin
    if lua_type(FState, -1) <> LUA_TTABLE then
    begin
      lua_pop(FState, 1); // Clean up the stack
      Exit(False); // Not a table, so the field doesn't exist
    end;

    // Navigate to the next field
    lua_getfield(FState, -1, AsUTF8(LTokens[I]));
    lua_remove(FState, -2); // Remove the parent table
  end;

  // If the final value is not nil, the variable exists
  Result := lua_type(FState, -1) <> LUA_TNIL;

  // Clean up the stack
  lua_pop(FState, 1);
end;

procedure TChandra.SetVariable(const AName: string; const AValue: TValue);
var
  LTokens: TArray<string>;
  I: Integer;
  LFinalKey: string;
  LType: Integer;
begin
  // Split the variable name into components for nested table navigation
  LTokens := AName.Split(['.']);
  if Length(LTokens) = 0 then
    raise EChandraRuntimeException.Create('Invalid variable name');

  // Get or create the base variable
  LType := lua_getglobal(FState, AsUTF8(LTokens[0])); // Push base variable onto the stack
  if LType = LUA_TNIL then
  begin
    // Base variable does not exist; create a new table
    lua_pop(FState, 1); // Remove nil
    lua_newtable(FState); // Create a new table
    lua_setglobal(FState, AsUTF8(LTokens[0])); // Assign the table globally
    lua_getglobal(FState, AsUTF8(LTokens[0])); // Push the new table onto the stack

    // Navigate through the nested fields
    for I := 1 to High(LTokens) - 1 do
    begin
      lua_getfield(FState, -1, AsUTF8(LTokens[I])); // Get the next field
      if lua_type(FState, -1) = LUA_TNIL then
      begin
        lua_pop(FState, 1); // Remove nil
        lua_newtable(FState); // Create a new table
        lua_pushvalue(FState, -1); // Duplicate the new table
        lua_setfield(FState, -3, AsUTF8(LTokens[I])); // Assign the new table to the parent
      end;
      lua_remove(FState, -2); // Remove the parent table
    end;

    // Push the value to set
    if not PushValueToLua(AValue) then
    begin
      lua_pop(FState, 1); // Clean up the stack
      raise EChandraRuntimeException.CreateFmt('Unsupported value for "%s"', [AName]);
    end;

    // Set the final field
    LFinalKey := LTokens[High(LTokens)];
    lua_setfield(FState, -2, AsUTF8(LFinalKey)); // Assign the value to the field
    lua_pop(FState, 1); // Remove the remaining table
  end
  else
  begin
    // If it's not nil, handle it differently based on its type
    if LType <> LUA_TTABLE then
    begin
      // Push the value directly for global variable assignment
      if not PushValueToLua(AValue) then
      begin
        lua_pop(FState, 1); // Clean up the stack
        raise EChandraRuntimeException.CreateFmt('Unsupported value for "%s"', [AName]);
      end;
      lua_setglobal(FState, AsUTF8(AName)); // Set the global variable
    end
    else
    begin
      // If it's a table, navigate and handle nested fields (same logic as for nil)
      for I := 1 to High(LTokens) - 1 do
      begin
        lua_getfield(FState, -1, AsUTF8(LTokens[I])); // Get the next field
        if lua_type(FState, -1) = LUA_TNIL then
        begin
          lua_pop(FState, 1); // Remove nil
          lua_newtable(FState); // Create a new table
          lua_pushvalue(FState, -1); // Duplicate the new table
          lua_setfield(FState, -3, AsUTF8(LTokens[I])); // Assign the new table to the parent
        end;
        lua_remove(FState, -2); // Remove the parent table
      end;

      // Push the value to set
      if not PushValueToLua(AValue) then
      begin
        lua_pop(FState, 1); // Clean up the stack
        raise EChandraRuntimeException.CreateFmt('Unsupported value for "%s"', [AName]);
      end;

      // Set the final field
      LFinalKey := LTokens[High(LTokens)];
      lua_setfield(FState, -2, AsUTF8(LFinalKey)); // Assign the value to the field
      lua_pop(FState, 1); // Remove the remaining table
    end;
  end;
end;

function TChandra.GetVariable(const AName: string): TValue;
var
  LScript: string;
  LStatus: Integer;
begin
  Result := TValue.Empty;

  // Check for valid variable
  if not VariableExist(AName) then Exit;

  // Construct the Lua script to get the variable
  LScript := Format('return %s', [AName]);

  // Load the script
  LStatus := luaL_loadstring(FState, AsUTF8(LScript));
  if LStatus <> LUA_OK then
    raise EChandraRuntimeException.CreateFmt('Invalid variable name "%s": %s', [AName, lua_tostring(FState, -1)]);

  // Execute the script
  LStatus := lua_pcall(FState, 0, 1, 0); // 0 arguments, 1 result
  if LStatus <> LUA_OK then
    raise EChandraRuntimeException.CreateFmt('Error retrieving variable "%s": %s', [AName, lua_tostring(FState, -1)]);

  // Check if the result is nil
  if lua_type(FState, -1) = LUA_TNIL then
  begin
    lua_pop(FState, 1); // Clean up the stack
    raise EChandraRuntimeException.CreateFmt('Variable "%s" does not exist', [AName]);
  end;

  // Convert the Lua value to a Delphi TValue
  Result := GetValueFromLua(-1);
  lua_pop(FState, 1); // Clean up the stack
end;

function TChandra.Call(const AName: string; const AParams: array of const): TValue;
var
  LScript: string;
  LParamStr: string;
  I: Integer;
  LStatus: Integer;
  LNumResults: Integer;
  LType: Integer;
begin
  Result := TValue.Empty;

  // Check if the function or construct exists
  lua_getglobal(FState, AsUTF8(AName));
  LType := lua_type(FState, -1);
  if not LType in [LUA_TFUNCTION, LUA_TTABLE, LUA_TUSERDATA, LUA_TNIL] then
  begin
    lua_pop(FState, 1); // Clean up the stack
    raise EChandraRuntimeException.CreateFmt('Invalid Lua construct "%s": not callable', [AName]);
  end;

  // Convert parameters into a comma-separated string
  LParamStr := '';
  for I := Low(AParams) to High(AParams) do
  begin
    if I > Low(AParams) then
      LParamStr := LParamStr + ', ';

    // Convert parameter to Lua-compatible representation
    LParamStr := LParamStr + LuaParamToString(AParams[I]);
  end;

  // Form the Lua script (wrap the function call in a return statement)
  LScript := Format('return %s(%s)', [AName, LParamStr]);

  // Load the script
  LStatus := luaL_loadstring(FState, AsUTF8(LScript));
  if LStatus <> LUA_OK then
  begin
    // Retrieve and raise the Lua error
    raise EChandraRuntimeException.CreateFmt('Error compiling Lua script: %s', [lua_tostring(FState, -1)]);
  end;

  // Execute the loaded script
  LNumResults := 1; // Expecting 1 result (function return value)
  LStatus := lua_pcall(FState, 0, LNumResults, 0);
  CheckLuaError(LStatus);

  // Retrieve the return value (if present)
  if lua_gettop(FState) > 0 then
  begin
    Result := GetValueFromLua(-1); // Extract value from the top of the stack
    lua_pop(FState, 1); // Remove the result from the stack
  end;

  // Clean up the Lua stack (ensure it's empty after execution)
  lua_settop(FState, 0);
end;

procedure TChandra.CompileToStream(const AFilename: string; const AStream: TStream; const ACleanOutput: Boolean);
var
  LInFilename: string;
  LBundleFilename: string;
begin
  if not Assigned(FState) then Exit;

  LInFilename := AFilename;
  LBundleFilename := TPath.GetFileNameWithoutExtension(LInFilename) + '_bundle.lua';
  LBundleFilename := TPath.Combine(TPath.GetDirectoryName(LInFilename), LBundleFilename);

  LInFilename := LInFilename.Replace('\', '/');
  LBundleFilename := LBundleFilename.Replace('\', '/');

  Bundle(LInFilename, LBundleFilename);
  LoadFile(PChar(LBundleFilename), False);
  SaveByteCode(AStream);
  lua_pop(FState, lua_gettop(FState));

  if ACleanOutput then
  begin
    if TFile.Exists(LBundleFilename) then
    begin
      TFile.Delete(LBundleFilename);
    end;
  end;
end;

procedure TChandra.AddSearchPath(const APath: string);
var
  LPathToAdd: string;
  LCurrentPath: string;
  LPath: string;
begin
  if not Assigned(FState) then Exit;

  LPath := APath;
  LPath := LPath.Replace('\', '/');

  // Check if APath already ends with "?.lua"
  if LPath.EndsWith('?.lua') then
    LPathToAdd := APath
  else
    LPathToAdd := IncludeTrailingPathDelimiter(LPath) + '?.lua';

  // Retrieve the current package.path
  lua_getglobal(FState, 'package'); // Get the "package" table
  if not lua_istable(FState, -1) then
    raise Exception.Create('"package" is not a table in the Lua state');

  lua_getfield(FState, -1, 'path'); // Get the "package.path" field
  if LongBool(lua_isstring(FState, -1)) then
    LCurrentPath := string(lua_tostring(FState, -1))
  else
    LCurrentPath := ''; // Default to empty if "path" is not set

  lua_pop(FState, 1); // Pop the "package.path" field

  // Check if the path is already included
  if Pos(LPathToAdd, LCurrentPath) = 0 then
  begin
    // Append the new path if not already included
    LCurrentPath := LPathToAdd + ';' + LCurrentPath;

    // Update package.path
    lua_pushstring(FState, AsUTF8(LCurrentPath)); // Push the updated path
    lua_setfield(FState, -2, 'path'); // Update "package.path"
  end;

  lua_pop(FState, 1); // Pop the "package" table
end;

procedure TChandra.Print(const AText: string; const AArgs: array of const);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(AText, AArgs));
end;

procedure TChandra.PrintLn(const AText: string; const AArgs: array of const);
begin
  if not HasConsoleOutput() then Exit;
  WriteLn(Format(AText, AArgs));
end;

const
  PAYLOADID = 'fa12d33b4ed84bc6a6dc4c2fd07a31e8';

function TChandra.PayloadExist(): Boolean;
begin
  Result := False;
  if not Assigned(FState) then Exit;

  Result := ResourceExists(HInstance, PAYLOADID);
end;

function TChandra.StorePayload(const ASourceFilename, AEXEFilename: string): Boolean;
var
  LStream: TMemoryStream;
begin
  Result := False;
  if not Assigned(FState) then Exit;

  if not TFile.Exists(ASourceFilename) then Exit;
  if not TFile.Exists(AEXEFilename) then Exit;
  if not IsValidWin64PE(AEXEFilename) then Exit;

  LStream := TMemoryStream.Create();
  try
    CompileToStream(ASourceFilename, LStream, True);
    if LStream.Size > 0 then
    begin
      Result := AddResFromMemory(AEXEFilename, PAYLOADID, LStream.Memory, LStream.Size);
    end;
  finally
    LStream.Free();
  end;
end;

function TChandra.RunPayload(): Boolean;
var
  LResStream: TResourceStream;
  LErr: string;
  LRes: Integer;
begin
  Result := False;
  if not Assigned(FState) then Exit;

  if not PayloadExist() then Exit;

  Reset();

  LResStream := TResourceStream.Create(HInstance, PAYLOADID, RT_RCDATA);
  try
    LoadBuffer(LResStream.Memory, LResStream.Size, False);
    LResStream.Free();
    LResStream := nil;
  finally
    if Assigned(LResStream) then
      LResStream.Free();
  end;

  // Check if the stack has any values
  if lua_gettop(FState) = 0 then
    raise EChandraException.Create('Lua stack is empty. Nothing to run.');

  // Check if the top of the stack is a function
  if lua_type(FState, lua_gettop(FState)) <> LUA_TFUNCTION then
    raise EChandraException.Create('Top of the stack is not a callable function.');

  // Call the function on the stack
  LRes := lua_pcall(FState, 0, LUA_MULTRET, 0);

  // Handle errors from pcall
  if LRes <> LUA_OK then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EChandraException.Create(LErr);
  end;

  Result := True;
end;

procedure TChandra.OnBeforeReset();
begin
end;

procedure TChandra.OnAfterReset();
begin
end;

procedure TChandra.SetGCStepSize(const AStep: Integer);
begin
  FGCStep := AStep;
end;

function TChandra.GetGCStepSize(): Integer;
begin
  Result := FGCStep;
end;

function TChandra.GetGCMemoryUsed(): Integer;
begin
  Result := 0;
  if not Assigned(FState) then Exit;

  Result := lua_gc(FState, LUA_GCCOUNT, FGCStep);
end;

procedure TChandra.CollectGarbage();
begin
  if not Assigned(FState) then Exit;

  lua_gc(FState, LUA_GCSTEP, FGCStep);
end;



{$ENDREGION}

{$REGION ' CRUNTIME '}
const
  ucrtbase = 'ucrtbase.dll';
  ucrt = 'api-ms-win-crt-stdio-l1-1-0.dll';
  msvcrt = 'msvcrt.dll';

procedure __mingw_vsprintf; cdecl; external msvcrt name 'vsprintf';
procedure __mingw_vfprintf; cdecl; external msvcrt name 'vfprintf';

procedure feof; cdecl; external ucrtbase;

procedure __mingw_strtod; cdecl; external ucrt name 'strtod';
procedure system; cdecl; external ucrt;
procedure rename; cdecl; external ucrt;
procedure malloc; cdecl; external ucrt;
procedure exit; cdecl; external ucrt;
procedure memset; cdecl; external ucrt;
procedure __intrinsic_setjmpex; cdecl; external ucrt;
procedure _time64; cdecl; external ucrt;
procedure strcmp; cdecl; external ucrt;
procedure strlen; cdecl; external ucrt;
procedure memcpy; external ucrt;
procedure strchr; cdecl; external ucrt;
procedure longjmp; cdecl; external ucrt;
procedure abort; cdecl; external ucrt;
procedure floor; cdecl; external ucrt;
procedure memcmp; cdecl; external ucrt;
procedure strcoll; cdecl; external ucrt;
procedure strpbrk; cdecl; external ucrt;
procedure strcpy; cdecl; external ucrt;
procedure localeconv; cdecl; external ucrt;
procedure strspn; cdecl; external ucrt;
procedure strncmp; cdecl; external ucrt;
procedure _errno; cdecl; external ucrt;
procedure strerror; cdecl; external ucrt;
procedure fopen; cdecl; external ucrt;
procedure __acrt_iob_func; cdecl; external ucrt;
procedure freopen; cdecl; external ucrt;
procedure ferror; cdecl; external ucrt;
procedure fclose; cdecl; external ucrt;
procedure getc; cdecl; external ucrt;
procedure fread; cdecl; external ucrt;
procedure strstr; cdecl; external ucrt;
procedure realloc; cdecl; external ucrt;
procedure free; cdecl; external ucrt;
procedure fflush; cdecl; external ucrt;
procedure getenv; cdecl; external ucrt;
procedure pow; cdecl; external ucrt;
procedure fmod; cdecl; external ucrt;
procedure frexp; cdecl; external ucrt;
procedure ldexp; cdecl; external ucrt;
procedure fwrite; cdecl; external ucrt;
procedure fputs; cdecl; external ucrt;
procedure fputc; cdecl; external ucrt;
procedure isalnum; cdecl; external ucrt;
procedure toupper; cdecl; external ucrt;
procedure fgets; cdecl; external ucrt;
procedure memchr; cdecl; external ucrt;
procedure _popen; cdecl; external ucrt;
procedure tmpfile; cdecl; external ucrt;
procedure clearerr; cdecl; external ucrt;
procedure ungetc; cdecl; external ucrt;
procedure isspace; cdecl; external ucrt;
procedure isxdigit; cdecl; external ucrt;
procedure _pclose; cdecl; external ucrt;
procedure fseek; cdecl; external ucrt;
procedure ftell; cdecl; external ucrt;
procedure setvbuf; cdecl; external ucrt;
procedure acos; cdecl; external ucrt;
procedure atan2; cdecl; external ucrt;
procedure log; cdecl; external ucrt;
procedure tan; cdecl; external ucrt;
procedure clock; cdecl; external ucrt;
procedure _gmtime64; cdecl; external ucrt;
procedure _localtime64; cdecl; external ucrt;
procedure strftime; cdecl; external ucrt;
procedure _difftime64; cdecl; external ucrt;
procedure remove; cdecl; external ucrt;
procedure setlocale; cdecl; external ucrt;
procedure _mktime64; cdecl; external ucrt;
procedure tmpnam; cdecl; external ucrt;
procedure isalpha; cdecl; external ucrt;
procedure iscntrl; cdecl; external ucrt;
procedure tolower; cdecl; external ucrt;
procedure isgraph; cdecl; external ucrt;
procedure islower; cdecl; external ucrt;
procedure ispunct; cdecl; external ucrt;
procedure isupper; cdecl; external ucrt;
procedure strrchr; cdecl; external ucrt;
procedure asin; cdecl; external ucrt;
procedure ceil; cdecl; external ucrt;
procedure log10; cdecl; external ucrt;
{$ENDREGION}

{$REGION ' UNIT INIT '}
{ Initialization Section }
initialization
begin
  // Enable memory leak reporting on application shutdown in Delphi.
  // This helps identify any memory leaks that might occur while using this unit.
  // This directive has no effect in Free Pascal Compiler (FPC) as it is Delphi-specific.
  ReportMemoryLeaksOnShutdown := True;

  // Configure floating-point exception handling.
  // Sets the exception mask to include floating-point overflow and invalid operation exceptions.
  // This prevents runtime errors for certain floating-point operations that would normally trigger exceptions.
  SetExceptionMask(GetExceptionMask + [exOverflow, exInvalidOp]);

  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);
  EnableVirtualTerminalProcessing();
end;

{ Finalization Section }
finalization
begin
end;
{$ENDREGION}

end.
