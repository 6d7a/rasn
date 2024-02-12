use core::ops;

pub(crate) const MAX_OID_FIRST_OCTET: u32 = 2;
pub(crate) const MAX_OID_SECOND_OCTET: u32 = 39;

const fn is_valid_oid(slice: &[u32]) -> bool {
    !slice.is_empty() && slice[0] <= MAX_OID_FIRST_OCTET
}

/// A reference to a global unique identifier that identifies an concept, such
/// as a organisation, or encoding rules.
#[derive(Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Oid([u32]);

impl Oid {
    /// Creates a new reference to a object identifier from `slice`.
    ///
    /// Returns `None` if `vec` is empty or the first
    /// component is greater than 2.
    /// ```
    /// use rasn::types::Oid;
    ///
    /// let internet = Oid::new(&[1, 3, 6, 1]).unwrap();
    /// ```
    pub const fn new(slice: &[u32]) -> Option<&Self> {
        if is_valid_oid(slice) {
            Some(Self::new_unchecked(slice))
        } else {
            None
        }
    }

    /// Creates a new reference to a object identifier from `slice`.
    ///
    /// Panics if `vec` is empty or the first
    /// component is greater than 2.
    pub const fn const_new(oid: &'static [u32]) -> &'static Self {
        match Self::new(oid) {
            Some(oid) => oid,
            None => panic!("not a valid OID"),
        }
    }

    /// Creates a new mutable reference to a object identifier from `slice`.
    ///
    /// Returns `None` if `vec` is empty or the first
    /// component is greater than 2.
    /// ```
    /// use rasn::types::Oid;
    ///
    /// let internet = Oid::new(&[1, 3, 6, 1]).unwrap();
    /// ```
    pub fn new_mut(slice: &mut [u32]) -> Option<&mut Self> {
        if is_valid_oid(slice) {
            Some(Self::new_unchecked_mut(slice))
        } else {
            None
        }
    }

    /// Creates a new reference to a object identifier from `slice`.
    ///
    /// # Safety
    /// This allows you to create potentially invalid object identifiers which
    /// may affect encoding validity.
    pub const fn new_unchecked(slice: &[u32]) -> &Self {
        unsafe { &*(slice as *const [u32] as *const Self) }
    }

    /// Creates a new object identifier from `slice`.
    ///
    /// # Safety
    /// This allows you to create potentially invalid object identifiers which
    /// may affect encoding validity.
    pub fn new_unchecked_mut(slice: &mut [u32]) -> &mut Self {
        unsafe { &mut *(slice as *mut [u32] as *mut Self) }
    }
}

impl alloc::borrow::ToOwned for Oid {
    type Owned = ObjectIdentifier;

    fn to_owned(&self) -> Self::Owned {
        Self::Owned::new_unchecked(self.0.to_owned().into())
    }
}

impl AsRef<[u32]> for Oid {
    fn as_ref(&self) -> &[u32] {
        self.0.as_ref()
    }
}

impl PartialEq<[u32]> for Oid {
    fn eq(&self, rhs: &[u32]) -> bool {
        &self.0 == rhs
    }
}

impl<const N: usize> PartialEq<[u32; N]> for Oid {
    fn eq(&self, rhs: &[u32; N]) -> bool {
        &self.0 == rhs
    }
}

impl PartialEq<Oid> for [u32] {
    fn eq(&self, rhs: &Oid) -> bool {
        self == &rhs.0
    }
}

impl PartialEq<Oid> for ObjectIdentifier {
    fn eq(&self, rhs: &Oid) -> bool {
        *self.0 == rhs.0
    }
}

impl PartialEq<&Oid> for ObjectIdentifier {
    fn eq(&self, rhs: &&Oid) -> bool {
        *self.0 == rhs.0
    }
}

impl PartialEq<Oid> for &ObjectIdentifier {
    fn eq(&self, rhs: &Oid) -> bool {
        *self.0 == rhs.0
    }
}

impl<const N: usize> PartialEq<Oid> for [u32; N] {
    fn eq(&self, rhs: &Oid) -> bool {
        self == &rhs.0
    }
}

impl ops::Deref for Oid {
    type Target = [u32];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for Oid {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A global unique identifier that identifies an concept, such as a
/// organisation, or encoding rules. The "owned" version of [`Oid`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct ObjectIdentifier(alloc::borrow::Cow<'static, [u32]>);

impl ObjectIdentifier {
    /// Creates a new object identifier from `vec`.
    ///
    /// Returns `None` if `vec` contains less than two components or the first
    /// component is greater than 1.
    pub fn new(arcs: impl Into<alloc::borrow::Cow<'static, [u32]>>) -> Option<Self> {
        let arcs = arcs.into();
        is_valid_oid(&arcs).then_some(Self(arcs))
    }

    /// Creates a new object identifier from `vec`.
    ///
    /// # Safety
    /// This allows you to create potentially invalid object identifiers which
    /// may affect encoding validity.
    pub const fn new_unchecked(vec: alloc::borrow::Cow<'static, [u32]>) -> Self {
        Self(vec)
    }
}

impl AsRef<[u32]> for ObjectIdentifier {
    fn as_ref(&self) -> &[u32] {
        self.0.as_ref()
    }
}

impl alloc::borrow::Borrow<Oid> for ObjectIdentifier {
    fn borrow(&self) -> &Oid {
        self
    }
}

impl<'a> From<&'a Oid> for ObjectIdentifier {
    fn from(oid: &'a Oid) -> Self {
        alloc::borrow::ToOwned::to_owned(oid)
    }
}

impl ops::Deref for ObjectIdentifier {
    type Target = Oid;

    fn deref(&self) -> &Self::Target {
        Oid::new_unchecked(&self.0)
    }
}

impl ops::DerefMut for ObjectIdentifier {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Oid::new_unchecked_mut(self.0.to_mut())
    }
}

impl<const N: usize> PartialEq<ObjectIdentifier> for [u32; N] {
    fn eq(&self, rhs: &ObjectIdentifier) -> bool {
        self == &**rhs
    }
}

impl PartialEq<ObjectIdentifier> for Oid {
    fn eq(&self, rhs: &ObjectIdentifier) -> bool {
        self.0 == *rhs.0
    }
}

impl PartialEq<ObjectIdentifier> for &Oid {
    fn eq(&self, rhs: &ObjectIdentifier) -> bool {
        self.0 == *rhs.0
    }
}

impl PartialEq<[u32]> for ObjectIdentifier {
    fn eq(&self, rhs: &[u32]) -> bool {
        self.0 == rhs
    }
}

macro_rules! oids {
    ($($name:ident => $($num:literal),+ $(,)?);+ $(;)?) => {
        impl Oid {
            $(
                pub const $name: &'static Oid = Oid::const_new(&[$($num),+]);
            )+
        }
    }
}

// ITU-T object identifiers
oids! {
    ITU_T => 0;
    ITU_T_DATA_PSS_UCL_PILOT => 0, 9, 2342, 19200300, 100;
    ITU_T_DATA_PSS_UCL_PILOT_ATTRIBUTE_TYPE => 0, 9, 2342, 19200300, 100, 1;
    ITU_T_DATA_PSS_UCL_PILOT_ATTRIBUTE_TYPE_DOMAIN_COMPONENT => 0, 9, 2342, 19200300, 100, 1, 25;
}

// ISO object identifiers
oids! {
    ISO => 1;

    ISO_MEMBER_BODY => 1, 2;
    ISO_MEMBER_BODY_US => 1, 2, 840;
    ISO_MEMBER_BODY_US_RSADSI => 1, 2, 840, 113549;
    ISO_MEMBER_BODY_US_X957_X9CM_DSA => 1, 2, 840, 10040, 4, 1;
    ISO_MEMBER_BODY_US_X957_X9CM_DSA_SHA1 => 1, 2, 840, 10040, 4, 3;

    ISO_MEMBER_BODY_US_ANSI_X942_NUMBER_TYPE_PUBLIC => 1, 2, 840, 10046, 2, 1;

    ISO_MEMBER_BODY_US_RSADSI_PKCS => 1, 2, 840, 113549, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS1 => 1, 2, 840, 113549, 1, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS1_RSA => 1, 2, 840, 113549, 1, 1, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS1_MD5_RSA => 1, 2, 840, 113549, 1, 1, 4;
    ISO_MEMBER_BODY_US_RSADSI_PKCS1_SHA1_RSA => 1, 2, 840, 113549, 1, 1, 5;
    ISO_MEMBER_BODY_US_RSADSI_PKCS1_RSAES_OAEP => 1, 2, 840, 113549, 1, 1, 7;

    ISO_MEMBER_BODY_US_RSADSI_DIGEST_ALGORITHM => 1, 2, 840, 113549, 1, 2;
    ISO_MEMBER_BODY_US_RSADSI_DIGEST_ALGORITHM_MD5 => 1, 2, 840, 113549, 1, 2, 5;

    ISO_MEMBER_BODY_US_RSADSI_ENCRYPTION_ALGORITHM => 1, 2, 840, 113549, 1, 3;
    ISO_MEMBER_BODY_US_RSADSI_ENCRYPTION_ALGORITHM_RC2_CBC => 1, 2, 840, 113549, 1, 3, 2;
    ISO_MEMBER_BODY_US_RSADSI_ENCRYPTION_ALGORITHM_DES_EDE3_CBC => 1, 2, 840, 113549, 1, 3, 7;

    ISO_MEMBER_BODY_US_RSADSI_PKCS5_PBKDF2 => 1, 2, 840, 113549, 1, 5, 12;

    ISO_MEMBER_BODY_US_RSADSI_PKCS7_DATA => 1, 2, 840, 113549, 1, 7, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS7_SIGNED_DATA => 1, 2, 840, 113549, 1, 7, 2;
    ISO_MEMBER_BODY_US_RSADSI_PKCS7_ENVELOPED_DATA => 1, 2, 840, 113549, 1, 7, 3;
    ISO_MEMBER_BODY_US_RSADSI_PKCS7_DIGESTED_DATA => 1, 2, 840, 113549, 1, 7, 5;
    ISO_MEMBER_BODY_US_RSADSI_PKCS7_ENCRYPTED_DATA => 1, 2, 840, 113549, 1, 7, 6;

    ISO_MEMBER_BODY_US_RSADSI_PKCS9 => 1, 2, 840, 113549, 1, 9;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_CONTENT_TYPE => 1, 2, 840, 113549, 1, 9, 3;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_MESSAGE_DIGEST => 1, 2, 840, 113549, 1, 9, 4;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SIGNING_TIME => 1, 2, 840, 113549, 1, 9, 5;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_COUNTER_SIGNATURE => 1, 2, 840, 113549, 1, 9, 6;

    ISO_MEMBER_BODY_US_RSADSI_PKCS9_CAPABILITIES => 1, 2, 840, 113549, 1, 9, 15;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME => 1, 2, 840, 113549, 1, 9, 16;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT => 1, 2, 840, 113549, 1, 9, 16, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_RECEIPT => 1, 2, 840, 113549, 1, 9, 16, 1, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_AUTHENTICATED_DATA => 1, 2, 840, 113549, 1, 9, 16, 1, 2;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_CONTENTINFO => 1, 2, 840, 113549, 1, 9, 16, 1, 6;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_FIRMWARE_PACKAGE => 1, 2, 840, 113549, 1, 9, 16, 1, 16;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_FIRMWARE_LOAD_RECEIPT => 1, 2, 840, 113549, 1, 9, 16, 1, 17;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_FIRMWARE_LOAD_ERROR => 1, 2, 840, 113549, 1, 9, 16, 1, 18;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_AUTH_ENVELOPED_DATA => 1, 2, 840, 113549, 1, 9, 16, 1, 23;
     ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CT_TRUST_ANCHOR_LIST => 1, 2, 840, 113549, 1, 9, 16, 1, 24;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA => 1, 2, 840, 113549, 1, 9, 16, 2;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_RECEIPT_REQUEST => 1, 2, 840, 113549, 1, 9, 16, 2, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_SECURITY_LABEL => 1, 2, 840, 113549, 1, 9, 16, 2, 2;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_ML_EXPAND_HISTORY => 1, 2, 840, 113549, 1, 9, 16, 2, 3;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_CONTENT_HINT => 1, 2, 840, 113549, 1, 9, 16, 2, 4;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_MESSAGE_SIGNATURE_DIGEST => 1, 2, 840, 113549, 1, 9, 16, 2, 5;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_CONTENT_IDENTIFIER => 1, 2, 840, 113549, 1, 9, 16, 2, 7;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_EQUIVALVENT_LABELS => 1, 2, 840, 113549, 1, 9, 16, 2, 9;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_CONTENT_REFERENCE => 1, 2, 840, 113549, 1, 9, 16, 2, 10;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_ENCRYPTION_KEY_PREFERENCE => 1, 2, 840, 113549, 1, 9, 16, 2, 11;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_SIGNING_CERTIFICATE => 1, 2, 840, 113549, 1, 9, 16, 2, 12;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_FIRMWARE_PACKAGE_ID => 1, 2, 840, 113549, 1, 9, 16, 2, 35;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_TARGET_HARDWARE_IDS => 1, 2, 840, 113549, 1, 9, 16, 2, 36;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_DECRYPT_KEY_ID => 1, 2, 840, 113549, 1, 9, 16, 2, 37;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_CRYPTO_ALGORITHMS => 1, 2, 840, 113549, 1, 9, 16, 2, 38;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_WRAPPED_FIRMWARE_KEY => 1, 2, 840, 113549, 1, 9, 16, 2, 39;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_COMMUNITY_IDENTIFIERS => 1, 2, 840, 113549, 1, 9, 16, 2, 40;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_FIRMWARE_PACKAGE_INFO => 1, 2, 840, 113549, 1, 9, 16, 2, 42;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_COMPRESS_ALGORITHMS => 1, 2, 840, 113549, 1, 9, 16, 2, 43;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_SIGNING_CERTIFICATE_V2 => 1, 2, 840, 113549, 1, 9, 16, 2, 47;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_AA_ASYMMETRIC_DECRYPT_KEY => 1, 2, 840, 113549, 1, 9, 16, 2, 54;

    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_ALGORITHM => 1, 2, 840, 113549, 1, 9, 16, 3;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_ALGORITHM_ESDH => 1, 2, 840, 113549, 1, 9, 16, 3, 5;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_ALGORITHM_CMS3DESWRAP => 1, 2, 840, 113549, 1, 9, 16, 3, 6;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_ALGORITHM_CMS3RC2WRAP => 1, 2, 840, 113549, 1, 9, 16, 3, 7;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_ALGORITHM_SSDH => 1, 2, 840, 113549, 1, 9, 16, 3, 10;

    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CAPABILITY => 1, 2, 840, 113549, 1, 9, 16, 3, 11;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_CAPABILITY_PREFER_BINARY_INSIDE => 1, 2, 840, 113549, 1, 9, 16, 3, 11, 1;

    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD => 1, 2, 840, 113549, 1, 9, 16, 8;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_USE_KEK => 1, 2, 840, 113549, 1, 9, 16, 8, 1;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_GL_DELETE => 1, 2, 840, 113549, 1, 9, 16, 8, 2;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_ADD_MEMBER => 1, 2, 840, 113549, 1, 9, 16, 8, 3;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_DELETE_MEMBER => 1, 2, 840, 113549, 1, 9, 16, 8, 4;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_REKEY => 1, 2, 840, 113549, 1, 9, 16, 8, 5;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_ADD_OWNER => 1, 2, 840, 113549, 1, 9, 16, 8, 6;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_REMOVE_OWNER => 1, 2, 840, 113549, 1, 9, 16, 8, 7;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_GLKEY_COMPROMISE => 1, 2, 840, 113549, 1, 9, 16, 8, 8;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_GLKEY_REFRESH => 1, 2, 840, 113549, 1, 9, 16, 8, 9;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_QUERY_REQUEST => 1, 2, 840, 113549, 1, 9, 16, 8, 11;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_QUERY_RESPONSE => 1, 2, 840, 113549, 1, 9, 16, 8, 12;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_PROVIDE_CERT => 1, 2, 840, 113549, 1, 9, 16, 8, 13;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_MANAGE_CERT => 1, 2, 840, 113549, 1, 9, 16, 8, 14;
    ISO_MEMBER_BODY_US_RSADSI_PKCS9_SMIME_SKD_GLKEY => 1, 2, 840, 113549, 1, 9, 16, 8, 15;

    ISO_IDENTIFIED_ORGANISATION => 1, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD => 1, 3, 6;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET => 1, 3, 6, 1;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_DIRECTORY => 1, 3, 6, 1, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_DIRECTORY_X509 => 1, 3, 6, 1, 1, 15;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT => 1, 3, 6, 1, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB => 1, 3, 6, 1, 2, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_SYSTEM => 1, 3, 6, 1, 2, 1, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_INTERFACES => 1, 3, 6, 1, 2, 1, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_AT => 1, 3, 6, 1, 2, 1, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_IP => 1, 3, 6, 1, 2, 1, 4;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_ICMP => 1, 3, 6, 1, 2, 1, 5;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_TCP => 1, 3, 6, 1, 2, 1, 6;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_UDP => 1, 3, 6, 1, 2, 1, 7;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_EGP => 1, 3, 6, 1, 2, 1, 8;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_CMOT => 1, 3, 6, 1, 2, 1, 9;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_TRANSMISSION => 1, 3, 6, 1, 2, 1, 10;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_MGMT_MIB_SNMP => 1, 3, 6, 1, 2, 1, 11;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_EXPERIMENTAL => 1, 3, 6, 1, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE => 1, 3, 6, 1, 4;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES => 1, 3, 6, 1, 3, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_WALL => 1, 3, 6, 1, 3, 1, 1466;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_WALL_DYN_EXT => 1, 3, 6, 1, 3, 1, 1466, 101, 119;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_WALL_ATTR => 1, 3, 6, 1, 3, 1, 1466, 101, 120;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_WALL_MATCH => 1, 3, 6, 1, 3, 1, 1466, 109, 114;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_WALL_SYNTAX => 1, 3, 6, 1, 3, 1, 1466, 115, 121, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_OPEN_LDAP_LDAP => 1, 3, 6, 1, 3, 1, 4203, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_OPEN_LDAP_LDAP_ATTRIBUTES => 1, 3, 6, 1, 3, 1, 4203, 1, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_OPEN_LDAP_LDAP_CONTROLS => 1, 3, 6, 1, 3, 1, 4203, 1, 10;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY => 1, 3, 6, 1, 5;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_KERBEROS_V5 => 1, 3, 6, 1, 5, 2;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS => 1, 3, 6, 1, 5, 5;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX => 1, 3, 6, 1, 5, 5, 7;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_PE => 1, 3, 6, 1, 5, 5, 7, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_PE_AUDIT_IDENTIFY => 1, 3, 6, 1, 5, 5, 7, 1, 4;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_PE_AA_CONTROLS => 1, 3, 6, 1, 5, 5, 7, 1, 6;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_PE_IP_ADDR_BLOCKS => 1, 3, 6, 1, 5, 5, 7, 1, 7;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_PE_AS_SYS_IDS => 1, 3, 6, 1, 5, 5, 7, 1, 8;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_PE_AC_PROXYING => 1, 3, 6, 1, 5, 5, 7, 1, 10;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_QT => 1, 3, 6, 1, 5, 5, 7, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_QT_CPS => 1, 3, 6, 1, 5, 5, 7, 2, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_QT_UNOTICE => 1, 3, 6, 1, 5, 5, 7, 2, 2;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP => 1, 3, 6, 1, 5, 5, 7, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP_SERVER_AUTH => 1, 3, 6, 1, 5, 5, 7, 3, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP_CLIENT_AUTH => 1, 3, 6, 1, 5, 5, 7, 3, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP_CODE_SIGNING => 1, 3, 6, 1, 5, 5, 7, 3, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP_EMAIL_PROTECTION => 1, 3, 6, 1, 5, 5, 7, 3, 4;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP_TIME_STAMPING => 1, 3, 6, 1, 5, 5, 7, 3, 8;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_KP_OCSP_SIGNING => 1, 3, 6, 1, 5, 5, 7, 3, 9;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_CMC => 1, 3, 6, 1, 5, 5, 7, 7;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_CMC_GLARR => 1, 3, 6, 1, 5, 5, 7, 7, 99;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_CMC_GLARR_SKD_ALG_REQUEST => 1, 3, 6, 1, 5, 5, 7, 7, 99, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_CMC_GLARR_SKD_ALG_RESPONSE => 1, 3, 6, 1, 5, 5, 7, 7, 99, 2;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ON => 1, 3, 6, 1, 5, 5, 7, 8;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ON_HARDWARE_MODULE_NAME => 1, 3, 6, 1, 5, 5, 7, 8, 4;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ACA => 1, 3, 6, 1, 5, 5, 7, 10;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ACA_AUTHENTICATION_INFO => 1, 3, 6, 1, 5, 5, 7, 10, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ACA_ACCESS_IDENTITY => 1, 3, 6, 1, 5, 5, 7, 10, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ACA_CHARGING_IDENTITY => 1, 3, 6, 1, 5, 5, 7, 10, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ACA_GROUP => 1, 3, 6, 1, 5, 5, 7, 10, 4;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_ACA_ENC_ATTRIBUTES => 1, 3, 6, 1, 5, 5, 7, 10, 6;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_CET => 1, 3, 6, 1, 5, 5, 7, 15;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_CET_SKD_FAIL_INFO => 1, 3, 6, 1, 5, 5, 7, 15, 1;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_AD => 1, 3, 6, 1, 5, 5, 7, 48;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_AD_OCSP => 1, 3, 6, 1, 5, 5, 7, 48, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_AD_CA_ISSUERS => 1, 3, 6, 1, 5, 5, 7, 48, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_AD_TIME_STAMPING => 1, 3, 6, 1, 5, 5, 7, 48, 3;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_PKIX_AD_CA_REPOSITORY => 1, 3, 6, 1, 5, 5, 7, 48, 5;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SECURITY_MECHANISMS_HMAC_SHA1 => 1, 3, 6, 1, 5, 5, 8, 1, 2;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SNMP_V2 => 1, 3, 6, 1, 6;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SNMP_V2_DOMAINS => 1, 3, 6, 1, 6, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SNMP_V2_PROXIES => 1, 3, 6, 1, 6, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_SNMP_V2_MODULES => 1, 3, 6, 1, 6, 3;

    ISO_IDENTIFIED_ORGANISATION_OIW_SECSIG_ALGORITHM_SHA1 => 1, 3, 14, 3, 2, 26;

    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_INDIRECT_DATA_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 1, 4;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_PE_IMAGE_DATA_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 1, 15;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_SP_OPUS_INFO_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 1, 12;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_STATEMENT_TYPE_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 1, 11;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_CAB_DATA_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 1, 25;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_SIPINFO_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 1, 30;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_PE_IMAGE_PAGE_HASHES_V1 => 1, 3, 6, 1, 4, 1, 311, 2, 3, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_PE_IMAGE_PAGE_HASHES_V2 => 1, 3, 6, 1, 4, 1, 311, 2, 3, 2;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_NESTED_SIGNATURE_OBJID => 1, 3, 6, 1, 4, 1, 311, 2, 4, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_TIME_STAMP_REQUEST_OBJID => 1, 3, 6, 1, 4, 1, 311, 3, 2, 1;
    ISO_IDENTIFIED_ORGANISATION_DOD_INTERNET_PRIVATE_ENTERPRISES_MICROSOFT_SPC_RFC3161_OBJID => 1, 3, 6, 1, 4, 1, 311, 3, 3, 1;
}

// Joint ISO-ITU-T object identifiers
oids! {
    JOINT_ISO_ITU_T => 2;

    JOINT_ISO_ITU_T_MEMBER_BODY => 2, 2;
    JOINT_ISO_ITU_T_MEMBER_BODY_US => 2, 2, 840;
    JOINT_ISO_ITU_T_MEMBER_BODY_US_X9CM => 2, 2, 840, 100400;
    JOINT_ISO_ITU_T_MEMBER_BODY_US_X9CM_HOLD_INSTRUCTION => 2, 2, 840, 100400, 2;
    JOINT_ISO_ITU_T_MEMBER_BODY_US_X9CM_HOLD_INSTRUCTION_NONE => 2, 2, 840, 100400, 2, 1;
    JOINT_ISO_ITU_T_MEMBER_BODY_US_X9CM_HOLD_INSTRUCTION_CALL_ISSUER => 2, 2, 840, 100400, 2, 2;
    JOINT_ISO_ITU_T_MEMBER_BODY_US_X9CM_HOLD_INSTRUCTION_REJECT => 2, 2, 840, 100400, 2, 3;

    JOINT_ISO_ITU_T_DS => 2, 5;
    JOINT_ISO_ITU_T_DS_MODULE => 2, 5, 1;

    JOINT_ISO_ITU_T_DS_MODULE_USEFUL_DEFINITIONS => 2, 5, 1, 0, 8;
    JOINT_ISO_ITU_T_DS_MODULE_INFORMATION_FRAMEWORK => 2, 5, 1, 1, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_ABSTRACT_SERVICE => 2, 5, 1, 2, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DISTRIBUTED_OPERATIONS => 2, 5, 1, 3, 8;
    JOINT_ISO_ITU_T_DS_MODULE_PROTOCOL_OBJECT_IDENTIFIERS => 2, 5, 1, 4, 8;
    JOINT_ISO_ITU_T_DS_MODULE_SELECTED_ATTRIBUTE_TYPES => 2, 5, 1, 5, 8;
    JOINT_ISO_ITU_T_DS_MODULE_SELECTED_OBJECT_CLASSES => 2, 5, 1, 6, 8;
    JOINT_ISO_ITU_T_DS_MODULE_AUTHENTICATION_FRAMEWORK => 2, 5, 1, 7, 8;
    JOINT_ISO_ITU_T_DS_MODULE_ALGORITHM_OBJECT_IDENTIFIERS => 2, 5, 1, 8, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_OBJECT_IDENTIFIERS => 2, 5, 1, 9, 8;
    JOINT_ISO_ITU_T_DS_MODULE_UPPER_BOUNDS => 2, 5, 1, 10, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DAP => 2, 5, 1, 11, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DSP => 2, 5, 1, 12, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DISTRIBUTED_DIRECTORY_OIDS => 2, 5, 1, 13, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_SHADOW_OIDS => 2, 5, 1, 14, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_SHADOW_ABSTRACT_SERVICE => 2, 5, 1, 15, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DISP => 2, 5, 1, 16, 7;
    JOINT_ISO_ITU_T_DS_MODULE_DOP => 2, 5, 1, 17, 7;
    JOINT_ISO_ITU_T_DS_MODULE_OP_BINDING_MANAGEMENT => 2, 5, 1, 18, 8;
    JOINT_ISO_ITU_T_DS_MODULE_OP_BINDING_OIDS => 2, 5, 1, 19, 8;
    JOINT_ISO_ITU_T_DS_MODULE_HIERARCHICAL_OPERATIONAL_BINDINGS => 2, 5, 1, 20, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DSA_OPERATIONAL_ATTRIBUTE_TYPES => 2, 5, 1, 22, 8;
    JOINT_ISO_ITU_T_DS_MODULE_SCHEMA_ADMINISTRATION => 2, 5, 1, 23, 8;
    JOINT_ISO_ITU_T_DS_MODULE_BASIC_ACCESS_CONTROL => 2, 5, 1, 24, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_OPERATIONAL_BINDING_TYPES => 2, 5, 1, 25, 8;
    JOINT_ISO_ITU_T_DS_MODULE_CERTIFICATE_EXTENSIONS => 2, 5, 1, 26, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_MANAGEMENT => 2, 5, 1, 27, 8;
    JOINT_ISO_ITU_T_DS_MODULE_ENHANCED_SECURITY => 2, 5, 1, 28, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_SECURITY_EXCHANGES => 2, 5, 1, 29, 8;
    JOINT_ISO_ITU_T_DS_MODULE_IDM_PROTOCOL_SPECIFICATION => 2, 5, 1, 30, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_IDM_PROTOCOLS => 2, 5, 1, 31, 8;
    JOINT_ISO_ITU_T_DS_MODULE_ATTRIBUTE_CERTIFICATE_DEFINITIONS => 2, 5, 1, 32, 8;
    JOINT_ISO_ITU_T_DS_MODULE_SERVICE_ADMINISTRATION => 2, 5, 1, 33, 8;
    JOINT_ISO_ITU_T_DS_MODULE_LDAP_ATTRIBUTES => 2, 5, 1, 34, 8;
    JOINT_ISO_ITU_T_DS_MODULE_COMMON_PROTOCOL_SPECIFICATION => 2, 5, 1, 35, 8;
    JOINT_ISO_ITU_T_DS_MODULE_OSI_PROTOCOL_SPECIFICATION => 2, 5, 1, 36, 8;
    JOINT_ISO_ITU_T_DS_MODULE_DIRECTORY_OSI_PROTOCOLS => 2, 5, 1, 37, 8;
    JOINT_ISO_ITU_T_DS_MODULE_LDAP_SYSTEM_SCHEMA => 2, 5, 1, 38, 8;
    JOINT_ISO_ITU_T_DS_MODULE_PASSWORD_POLICY => 2, 5, 1, 39, 8;
    JOINT_ISO_ITU_T_DS_MODULE_PKI_PMI_EXTERNAL_DATA_TYPES => 2, 5, 1, 40, 8;
    JOINT_ISO_ITU_T_DS_MODULE_EXTENSION_ATTRIBUTES => 2, 5, 1, 41, 8;
    JOINT_ISO_ITU_T_DS_MODULE_PKI_PMI_WRAPPER => 2, 5, 1, 42, 8;
    JOINT_ISO_ITU_T_DS_MODULE_AVL_MANAGEMENT => 2, 5, 1, 43, 8;
    JOINT_ISO_ITU_T_DS_MODULE_TRUST_BROKER_PROTOCOL => 2, 5, 1, 44, 8;

    JOINT_ISO_ITU_T_DS_SERVICE_ELEMENT => 2, 5, 2;
    JOINT_ISO_ITU_T_DS_APPLICATION_CONTEXT => 2, 5, 3;

    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE => 2, 5, 4;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_COMMON_NAME => 2, 5, 4, 3;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_SURNAME => 2, 5, 4, 4;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_SERIAL_NUMBER => 2, 5, 4, 5;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_COUNTRY_NAME => 2, 5, 4, 6;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_LOCALITY_NAME => 2, 5, 4, 7;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_STATE_OR_PROVINCE_NAME => 2, 5, 4, 8;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_ORGANISATION_NAME => 2, 5, 4, 10;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_ORGANISATIONAL_UNIT_NAME => 2, 5, 4, 11;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_TITLE => 2, 5, 4, 12;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_NAME => 2, 5, 4, 41;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_GIVEN_NAME => 2, 5, 4, 42;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_INITIALS => 2, 5, 4, 43;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_GENERATION_QUALIFIER => 2, 5, 4, 44;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_DN_QUALIFIER => 2, 5, 4, 46;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_CLEARANCE => 2, 5, 4, 55;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_PSEUDONYM => 2, 5, 4, 65;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_TYPE_ROLE => 2, 5, 4, 72;

    JOINT_ISO_ITU_T_DS_SYNTAX_VENDOR => 2, 5, 5;
    JOINT_ISO_ITU_T_DS_OBJECT_CLASS => 2, 5, 6;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_SET => 2, 5, 7;
    JOINT_ISO_ITU_T_DS_ALGORITHM => 2, 5, 8;
    JOINT_ISO_ITU_T_DS_ABSTRACT_SYNTAX => 2, 5, 9;
    JOINT_ISO_ITU_T_DS_OBJECT => 2, 5, 10;
    JOINT_ISO_ITU_T_DS_PORT => 2, 5, 11;
    JOINT_ISO_ITU_T_DS_DSA_OPERATIONAL_ATTRIBUTE => 2, 5, 12;
    JOINT_ISO_ITU_T_DS_MATCHING_RULE => 2, 5, 13;
    JOINT_ISO_ITU_T_DS_KNOWLEDGE_MATCHING_RULE => 2, 5, 14;
    JOINT_ISO_ITU_T_DS_NAME_FORM => 2, 5, 15;
    JOINT_ISO_ITU_T_DS_GROUP => 2, 5, 16;
    JOINT_ISO_ITU_T_DS_SUBENTRY => 2, 5, 17;
    JOINT_ISO_ITU_T_DS_OPERATIONAL_ATTRIBUTE_TYPE => 2, 5, 18;
    JOINT_ISO_ITU_T_DS_OPERATIONAL_BINDING => 2, 5, 19;
    JOINT_ISO_ITU_T_DS_SCHEMA_OBJECT_CLASS => 2, 5, 20;
    JOINT_ISO_ITU_T_DS_SCHEMA_OPERATIONAL_ATTRIBUTE => 2, 5, 21;
    JOINT_ISO_ITU_T_DS_ADMINISTRATIVE_ROLES => 2, 5, 23;
    JOINT_ISO_ITU_T_DS_ACCESS_CONTROL_ATTRIBUTE => 2, 5, 24;
    JOINT_ISO_ITU_T_DS_ROS_OBJECT => 2, 5, 25;
    JOINT_ISO_ITU_T_DS_CONTRACT => 2, 5, 26;
    JOINT_ISO_ITU_T_DS_PACKAGE => 2, 5, 27;
    JOINT_ISO_ITU_T_DS_ACCESS_CONTROL_SCHEMES => 2, 5, 28;

    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION => 2, 5, 29;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_AUTHORITY_INFO_ACCESS => 2, 5, 29, 1;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_SUBJECT_DIRECTORY_ATTRIBUTES => 2, 5, 29, 9;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_SUBJECT_INFO_ACCESS => 2, 5, 29, 11;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_SUBJECT_KEY_IDENTIFIER => 2, 5, 29, 14;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_KEY_USAGE => 2, 5, 29, 15;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_PRICATE_KEY_USAGE_PERIOD => 2, 5, 29, 16;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_SUBJECT_ALT_NAME => 2, 5, 29, 17;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_ISSUER_ALT_NAME => 2, 5, 29, 18;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_BASIC_CONSTRAINTS => 2, 5, 29, 19;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_CRL_NUMBER => 2, 5, 29, 20;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_CRL_REASONS => 2, 5, 29, 21;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_HOLD_INSTRUCTION_CODE => 2, 5, 29, 23;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_INVALIDITY_DATE => 2, 5, 29, 24;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_DELTA_CRL_INDICATOR => 2, 5, 29, 27;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_ISSUING_DISTRIBUTION_POINT => 2, 5, 29, 28;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_CERTIFICATE_ISSUER => 2, 5, 29, 29;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_AUTHORITY_CRL_NAME_CONSTRAINTS => 2, 5, 29, 30;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_AUTHORITY_CRL_DISTRIBUTION_POINTS => 2, 5, 29, 31;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_CERTIFICATE_POLICIES => 2, 5, 29, 32;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_CERTIFICATE_POLICIES_ANY_POLICY => 2, 5, 29, 32, 0;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_POLICY_MAPPINGS => 2, 5, 29, 33;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_AUTHORITY_KEY_IDENTIFIER => 2, 5, 29, 35;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_AUTHORITY_EXT_KEY_USAGE => 2, 5, 29, 37;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_AUTHORITY_EXT_KEY_USAGE_ANY_EXTENDED_KEY_USAGE => 2, 5, 29, 37, 0;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_FRESHEST_CRL => 2, 5, 29, 46;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_INHIBIT_ANY_POLICY => 2, 5, 29, 54;
    JOINT_ISO_ITU_T_DS_CERTIFICATE_EXTENSION_TARGET_INFORMATION => 2, 5, 29, 55;

    JOINT_ISO_ITU_T_DS_MANAGEMENT_OBJECT => 2, 5, 30;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_VALUE_CONTEXT => 2, 5, 31;
    JOINT_ISO_ITU_T_DS_SECURITY_EXCHANGE => 2, 5, 32;
    JOINT_ISO_ITU_T_DS_IDM_PROTOCOL => 2, 5, 33;
    JOINT_ISO_ITU_T_DS_PROBLEM => 2, 5, 34;
    JOINT_ISO_ITU_T_DS_NOTIFICATION => 2, 5, 35;
    JOINT_ISO_ITU_T_DS_MATCHING_RESTRICTION => 2, 5, 36;
    JOINT_ISO_ITU_T_DS_CONTROL_ATTRIBUTE_TYPE => 2, 5, 37;
    JOINT_ISO_ITU_T_DS_KEY_PURPOSES => 2, 5, 38;
    JOINT_ISO_ITU_T_DS_PASSWORD_QUALITY => 2, 5, 39;
    JOINT_ISO_ITU_T_DS_ATTRIBUTE_SYNTAX => 2, 5, 40;
    JOINT_ISO_ITU_T_DS_AV_RESTRICTION => 2, 5, 41;
    JOINT_ISO_ITU_T_DS_CMS_CONTENT_TYPE => 2, 5, 42;

    JOINT_ISO_ITU_T_COUNTRY => 2, 16;
    JOINT_ISO_ITU_T_COUNTRY_US => 2, 16, 840;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION => 2, 16, 840, 1;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV => 2, 16, 840, 1, 101;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR => 2, 16, 840, 1, 101, 3;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS => 2, 16, 840, 1, 101, 3, 4;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES => 2, 16, 840, 1, 101, 3, 4, 1;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES128_CBC => 2, 16, 840, 1, 101, 3, 4, 1, 2;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES128_WRAP => 2, 16, 840, 1, 101, 3, 4, 1, 5;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES192_CBC => 2, 16, 840, 1, 101, 3, 4, 1, 22;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES192_WRAP => 2, 16, 840, 1, 101, 3, 4, 1, 25;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES256_CBC => 2, 16, 840, 1, 101, 3, 4, 1, 42;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_AES256_WRAP => 2, 16, 840, 1, 101, 3, 4, 1, 45;

    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_HASH_SHA224 => 2, 16, 840, 1, 101, 3, 4, 2, 4;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_HASH_SHA256 => 2, 16, 840, 1, 101, 3, 4, 2, 1;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_HASH_SHA384 => 2, 16, 840, 1, 101, 3, 4, 2, 2;
    JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_HASH_SHA512 => 2, 16, 840, 1, 101, 3, 4, 2, 3;

    JOINT_ISO_ITU_T_REGISTRATION_PROCEDURES_MODULE_DIRECTORY_DEFS => 2, 17, 1, 2;
}

#[cfg(test)]
mod test {
    use super::ObjectIdentifier;
    use super::Oid;

    #[test]
    fn transmute() {
        let mut oid = ObjectIdentifier::new_unchecked(alloc::vec![1, 3, 6].into());

        assert_eq!([1u32, 3, 6][..], *oid);
        oid.reverse();
        assert_eq!([6u32, 3, 1][..], *oid);
    }

    #[test]
    fn partial_eq() {
        let oid =
            ObjectIdentifier::new_unchecked(alloc::vec![2, 16, 840, 1, 101, 3, 4, 2, 3].into());
        assert_eq!(
            Oid::JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_HASH_SHA512,
            oid
        );
        assert_eq!(
            Oid::JOINT_ISO_ITU_T_COUNTRY_US_ORGANIZATION_GOV_CSOR_NIST_ALGORITHMS_HASH_SHA512,
            oid
        );
        assert_eq!(
            ObjectIdentifier::new(vec![1, 2]).unwrap(),
            Oid::ISO_MEMBER_BODY
        );
        assert_eq!(
            Oid::ISO_MEMBER_BODY,
            ObjectIdentifier::new(vec![1, 2]).unwrap()
        );
    }
}
