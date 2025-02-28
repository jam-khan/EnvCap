import React, { useState } from 'react';

export default function Playground() {
  const [code, setCode] = useState('// Write your ENVCAP code here');
  const [output, setOutput] = useState('');

  const runCode = () => {
    // Add logic to evaluate ENVCAP code
    setOutput('Output will appear here');
  };

  return (
    <div>
      <textarea value={code} onChange={(e) => setCode(e.target.value)} />
      <button onClick={runCode}>Run</button>
      <pre>{output}</pre>
    </div>
  );
}