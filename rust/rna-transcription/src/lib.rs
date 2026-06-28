use std::borrow::Cow;

const RNA_NUCLEOTIDES: &str = "AUGC";
const DNA_NUCLEOTIDES: &str = "ATGC";

#[derive(Debug, PartialEq, Eq)]
pub struct Dna<'a> {
    seq: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Rna<'a> {
    seq: Cow<'a, str>,
}

impl<'a> Dna<'a> {
    pub fn new(dna: &'a str) -> Result<Dna<'a>, usize> {
        if let Some((i, _)) = dna
            .chars()
            .enumerate()
            .find(|(_, c)| !DNA_NUCLEOTIDES.contains(*c))
        {
            Err(i)
        } else {
            Ok(Dna { seq: dna })
        }
    }

    pub fn into_rna(self) -> Rna<'a> {
        let seq = self
            .seq
            .chars()
            .map(|c| match c {
                'A' => 'U',
                'T' => 'A',
                'G' => 'C',
                'C' => 'G',
                _ => panic!("unrepresentable"),
            })
            .collect::<String>();
        Rna {
            seq: Cow::Owned(seq),
        }
    }
}

impl<'a> Rna<'a> {
    pub fn new(rna: &'a str) -> Result<Rna<'a>, usize> {
        if let Some((i, _)) = rna
            .chars()
            .enumerate()
            .find(|(_, c)| !RNA_NUCLEOTIDES.contains(*c))
        {
            Err(i)
        } else {
            Ok(Rna {
                seq: Cow::Borrowed(rna),
            })
        }
    }
}
