use super::VyperCompilationError;
use crate::artifacts::{BytecodeObject, Contract, Evm, FileToContractsMap, SourceFile};
use alloy_json_abi::JsonAbi;
use alloy_primitives::Bytes;
use serde::Deserialize;
use std::{
    collections::{BTreeMap, HashSet},
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Deserialize)]
pub struct Bytecode {
    pub object: Bytes,
    /// Opcodes list (string)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub opcodes: Option<String>,
}

impl From<Bytecode> for crate::artifacts::Bytecode {
    fn from(bytecode: Bytecode) -> Self {
        crate::artifacts::Bytecode {
            object: BytecodeObject::Bytecode(bytecode.object),
            opcodes: bytecode.opcodes,
            function_debug_data: Default::default(),
            generated_sources: Default::default(),
            source_map: Default::default(),
            link_references: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeployedBytecode {
    #[serde(flatten)]
    pub bytecode: Option<Bytecode>,
    #[serde(default, deserialize_with = "deserialize_vyper_sourcemap")]
    pub source_map: Option<String>,
}

impl From<DeployedBytecode> for crate::artifacts::DeployedBytecode {
    fn from(deployed_bytecode: DeployedBytecode) -> Self {
        crate::artifacts::DeployedBytecode {
            bytecode: deployed_bytecode.bytecode.map(Into::into),
            immutable_references: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VyperEvm {
    #[serde(default)]
    pub bytecode: Option<Bytecode>,
    #[serde(default)]
    pub deployed_bytecode: Option<DeployedBytecode>,
    /// The list of function hashes
    #[serde(default)]
    pub method_identifiers: BTreeMap<String, String>,
}

impl From<VyperEvm> for Evm {
    fn from(evm: VyperEvm) -> Self {
        Evm {
            bytecode: evm.bytecode.map(Into::into),
            deployed_bytecode: evm.deployed_bytecode.map(Into::into),
            method_identifiers: evm.method_identifiers,
            assembly: None,
            legacy_assembly: None,
            gas_estimates: None,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct VyperContract {
    /// Contract ABI.
    pub abi: Option<JsonAbi>,
    /// EVM-related outputs
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub evm: Option<VyperEvm>,
}

impl From<VyperContract> for Contract {
    fn from(contract: VyperContract) -> Self {
        Contract {
            abi: contract.abi,
            evm: contract.evm.map(Into::into),
            metadata: None,
            userdoc: Default::default(),
            devdoc: Default::default(),
            ir: None,
            storage_layout: Default::default(),
            ewasm: None,
            ir_optimized: None,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct VyperSourceFile {
    pub id: u32,
}

impl From<VyperSourceFile> for SourceFile {
    fn from(source: VyperSourceFile) -> Self {
        SourceFile { id: source.id, ast: None }
    }
}

/// Vyper compiler output
#[derive(Debug, Deserialize)]
pub struct VyperOutput {
    #[serde(default = "Vec::new", skip_serializing_if = "Vec::is_empty")]
    pub errors: Vec<VyperCompilationError>,
    #[serde(default)]
    pub contracts: FileToContractsMap<VyperContract>,
    #[serde(default)]
    pub sources: BTreeMap<PathBuf, VyperSourceFile>,
}

impl VyperOutput {
    /// Retains only those files the given iterator yields
    ///
    /// In other words, removes all contracts for files not included in the iterator
    pub fn retain_files<'a, I>(&mut self, files: I)
    where
        I: IntoIterator<Item = &'a Path>,
    {
        // Note: use `to_lowercase` here because vyper not necessarily emits the exact file name,
        // e.g. `src/utils/upgradeProxy.sol` is emitted as `src/utils/UpgradeProxy.sol`
        let files: HashSet<_> =
            files.into_iter().map(|s| s.to_string_lossy().to_lowercase()).collect();
        self.contracts.retain(|f, _| files.contains(&f.to_string_lossy().to_lowercase()));
        self.sources.retain(|f, _| files.contains(&f.to_string_lossy().to_lowercase()));
    }
}

impl From<VyperOutput> for super::CompilerOutput<VyperCompilationError> {
    fn from(output: VyperOutput) -> Self {
        super::CompilerOutput {
            errors: output.errors,
            contracts: output
                .contracts
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().map(|(k, v)| (k, v.into())).collect()))
                .collect(),
            sources: output.sources.into_iter().map(|(k, v)| (k, v.into())).collect(),
        }
    }
}

/// Before Vyper 0.4 source map was represented as a string, after 0.4 it is represented as a map
/// where compressed source map is stored under `pc_pos_map_compressed` key.
fn deserialize_vyper_sourcemap<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum SourceMap {
        New { pc_pos_map_compressed: String },
        Old(String),
    }

    Ok(SourceMap::deserialize(deserializer).map_or(None, |v| {
        Some(match v {
            SourceMap::Old(s) => s,
            SourceMap::New { pc_pos_map_compressed } => pc_pos_map_compressed,
        })
    }))
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    fn test_output(artifact_path: &str) {
        let output = std::fs::read_to_string(
            Path::new(env!("CARGO_MANIFEST_DIR")).join("test-data").join(artifact_path),
        )
        .unwrap();
        let output: super::VyperOutput = serde_json::from_str(&output).unwrap();

        assert_eq!(output.contracts.len(), 2);
        assert_eq!(output.sources.len(), 2);

        let artifact = output.contracts.get(&PathBuf::from("src/a.vy")).unwrap().get("a").unwrap();
        assert!(artifact.evm.is_some());
        let evm = artifact.evm.as_ref().unwrap();
        let deployed_bytecode = evm.deployed_bytecode.as_ref().unwrap();
        assert!(deployed_bytecode.source_map.is_some());
    }

    #[test]
    fn can_deserialize_03_output() {
        test_output("sample-vyper-0.3-output.json");
    }

    #[test]
    fn can_deserialize_04_output() {
        test_output("sample-vyper-0.4-output.json");
    }
}
